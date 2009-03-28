%%----------------------------------------------------------------
%%% @author  Yomi Colledge <yomi@boodah.net>
%%% @doc Web interface for Chatterl
%%%
%%% Chatterl Web Gateway, allowing Web based clients to interact
%%% with Chatterl over a RESTful API.
%%%
%%% Allows Chatterl to interface with any web-based interface
%%% Using JSON and XML, sending the requests off to the chatterl_serv
%%% module.
%%%
%%% All calls to CWIGA will only be allowed via a specified IP, which
%%% will be defined with the configuration file.
%%% @end
%%% @copyright 2008-2009 Yomi Colledge
%%%---------------------------------------------------------------
-module(cwiga).

-behaviour(gen_server).

%% API
-export([start_link/1,stop/0]).

-define(APP, "CWIGA").
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start(Port) -> {ok,Pid} | ignore | {error,Error}
%% @end
%%--------------------------------------------------------------------
start_link(Port) ->
  gen_server:start_link({global, ?SERVER}, ?MODULE, [Port], []).

stop() ->
    gen_server:call({global, ?SERVER}, stop, infinity).

%%--------------------------------------------------------------------
%% @doc
%% Dispatches our requests to the relevant handle.
%%
%% Uses clean_path to determine what the action is.
%% @spec dispatch_requests(Request) -> void()
%% @end
%%--------------------------------------------------------------------
dispatch_requests(Req) ->
  [Path|Ext] = string:tokens(Req:get(path),"."),
  Method = Req:get(method),
  Post = Req:parse_post(),
  io:format("~p request for ~p with post: ~p~n", [Method, Path, Post]),
  Response = gen_server:call({global,?MODULE},{Method, Path, get_content_type(Ext), Post}),
  Req:respond(Response).

%%====================================================================
%% gen_server callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Initiates the server
%%
%% Function: init(Port) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}

%%--------------------------------------------------------------------
init([Port]) ->
  process_flag(trap_exit, true),
  mochiweb_http:start([{port, Port}, {loop, fun dispatch_requests/1}]),
  erlang:monitor(process,mochiweb_http),
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @doc
%% Description: Handling call messages
%%
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    io:format("Processing shut down ~s~n", [?APP]),
    {stop, normal, stopped, State};
handle_call({'POST',Url,ContentType,Post},_From,State) ->
  Reply = handle_response(handle_request('POST',Url,ContentType,Post),ContentType),
  {reply, Reply, State};
handle_call({'GET',"/users/connect/" ++ Client,ContentType,_Post},_From,State) ->
  Reply = handle_response(chatterl_mid_man:connect(ContentType,Client),ContentType),
  {reply, Reply, State};
handle_call({'GET',"/users/disconnect/" ++ Client,ContentType,_Post},_From,State) ->
  Reply = handle_response(chatterl_mid_man:disconnect(ContentType,Client),ContentType),
  {reply, Reply, State};
handle_call({'GET',"/users/list/" ++ Group,ContentType,_Post},_From,State) ->
  Reply = handle_response(chatterl_mid_man:user_list(ContentType,Group),ContentType),
  {reply, Reply, State};
handle_call({'GET',"/users/list",ContentType,_Post},_From,State) ->
  Reply = handle_response(chatterl_mid_man:user_list(ContentType),ContentType),
  {reply, Reply, State};
handle_call({'GET',"/users/poll/" ++ Client,ContentType,_Post},_From,State) ->
  Reply = handle_response(chatterl_mid_man:user_poll(ContentType,Client),ContentType),
  {reply, Reply, State};
handle_call({'GET',"/users/groups/" ++ Client,ContentType,_Post},_From,State) ->
  Reply = handle_response(chatterl_mid_man:user_groups(ContentType,Client),ContentType),
  {reply, Reply, State};
handle_call({'GET',"/groups/poll/" ++ Group,ContentType,_Post},_From,State) ->
  Reply = handle_response(chatterl_mid_man:group_poll(ContentType,Group),ContentType),
  {reply, Reply, State};
handle_call({'GET',"/groups/list",ContentType,_Post},_From,State) ->
  Reply = handle_response(chatterl_mid_man:group_list(ContentType),ContentType),
  {reply, Reply, State};
handle_call({'GET',"/groups/info/" ++ Group,ContentType,_Post},_From,State) ->
  Reply = handle_response(chatterl_mid_man:group_info(ContentType,Group),ContentType),
  {reply, Reply, State};
handle_call({_,Path,ContentType,_},_From,State) ->
  Response = message_handler:get_response_body(ContentType,
                                               message_handler:build_carrier("error", "Unknown command: " ++Path)),
  Reply = error(Response,ContentType),
  {reply, Reply, State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_request('POST',Url,ContentType,Post) ->
  case Url of
    "/register/" ++ Nick ->
      [{"name",Name},{"email",Email},{"pass1",Pass1},{"pass2",Pass2}] = Post,
      chatterl_mid_man:register(ContentType,{Nick,{Name,Email,Pass1,Pass2}});
    "/groups/send/" ++ Group ->
      [{"client",Sender},{"msg",Message}] = Post,
      chatterl_mid_man:group_send(ContentType,{Group,Sender,Message});
    "/groups/join/" ++ Group ->
      [{"client",Client}] = Post,
      chatterl_mid_man:group_join(ContentType,{Group,Client});
    "/groups/leave/" ++ Group ->
      [{"client",Client}] = Post,
      chatterl_mid_man:group_leave(ContentType,{Group,Client});
    "/users/login" ->
      [{"login",Login},{"pass",Pass}] = Post,
      chatterl_mid_man:login(ContentType,{Login,Pass});
    "/users/logout" ->
      [{"client",Client}] = Post,
      chatterl_mid_man:logout(ContentType,Client)
  end.
%%--------------------------------------------------------------------
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'DOWN', _Ref, _Process, {mochiweb_http, Host}, Reason}, State) ->
    io:format("Unable to start mochiweb on ~s:~nReason: ~s~n",[Host,Reason]),
    {stop,normal,State};
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% Terminates Chatterl Web Interface, making sure to shutdown mochiweb
%% along side it.
%%
%% @spec terminate({node,Reason},State) -> void()
%% @todo Needs a time out for when the port is already in use.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    io:format("Shutting down ChatterlWeb on: ~s...~n",[node(self())]),
    mochiweb_http:stop(),
    ok.

%%--------------------------------------------------------------------
%% @doc
%%
%% Convert process state when code is changed
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%%
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Gets the content type, used to help CWIGA to determine what format
%% to respond in.
%% @spec get_content_type(Type) -> string()
%%
%% @end
%%--------------------------------------------------------------------
get_content_type(Type) ->
    case Type of
	["json"] ->
	    ["text/json"];
	["xml"] ->
	    ["text/xml"];
	_ -> ["text/json"]
    end.

check_json_response(Json) ->
  {struct,[{<<"chatterl">>,{struct,[{<<"response">>,{struct,[Response]}}]}}]} = mochijson2:decode(Json),
  Response.

error(Response,ContentType) ->
  {404, [{"Content-Type", ContentType}], list_to_binary(Response)}.

failure(Response,ContentType) ->
  {501, [{"Content-Type", ContentType}], list_to_binary(Response)}.

success(Response,ContentType) ->
  {200, [{"Content-Type", ContentType}], list_to_binary(Response)}.


handle_response(Response,ContentType) ->
  case check_json_response(Response) of
    {<<"success">>,_} -> success(Response,ContentType);
    {<<"error">>,_} -> error(Response,ContentType);
    {<<"failure">>,_} -> failure(Response,ContentType)
  end.
