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
-module(chatterl_gateway).

-behaviour(gen_server).

%% API
-export([start/1,dispatch_requests/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).
%% Record used to pass our basic messages from the gateway to chatterl_web
-define(SERVER, ?MODULE).
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
start(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

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
  Action = clean_path(Path),
  handle(Action,get_content_type(Ext),Req).

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
    io:format("Initialising Chatterl Web Interface~n"),
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
handle_call(_Request, _From, State) ->
    {reply,ok,State}.

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
%% Cleans up a request so we only retrieve the path.
%% @spec clean_path(Path) -> [Path]
%%
%% @end
%%--------------------------------------------------------------------
clean_path(Path) ->
    case string:str(Path, "?") of
	0 ->
	    Path;
	N ->
	    string:substr(Path, 1, string:len(Path) - (N + 1))
    end.

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

%%--------------------------------------------------------------------
%% @doc
%%
%% Handles our RESTful resquests.
%%
%% @spec handle(Action,ContentType,Req) -> void()
%%
%% @end
%%--------------------------------------------------------------------
handle(Path,ContentType,Req) ->
  % Log Action
  %io:format("~s - Executing Path: ~s~n",[httpd_util:rfc1123_date(erlang:localtime()),Path]),
  Response =
    case Path of
      %% Client based requests
      "/connect/" ++ Client -> chatterl_mid_man:connect(ContentType,Client);
      "/disconnect/" ++ Client -> chatterl_mid_man:disconnect(ContentType,Client);
      "/users/list" -> chatterl_mid_man:user_list(ContentType);
      "/users/list/" ++Group -> chatterl_mid_man:user_list(ContentType,Group);
      "/users/send/" ++ Sender -> [Client, Message] = get_properties(Req,["client","msg"]),
                                  chatterl_mid_man:user_msg(ContentType,{Sender,Client,Message});
      "/users/poll/" ++ Client -> chatterl_mid_man:user_poll(ContentType,Client);
      "/users/groups/" ++ Client -> chatterl_mid_man:user_groups(ContentType,Client);
      %% Group based requests
      "/groups/list" -> chatterl_mid_man:group_list(ContentType);
      "/groups/info/" ++ Group -> chatterl_mid_man:group_info(ContentType,Group);
      "/groups/join/" ++ Group -> [Client] = get_properties(Req,["client"]),
                                  chatterl_mid_man:group_join(ContentType,{Group,Client});
      "/groups/leave/" ++ Group -> [Client] = get_properties(Req,["client"]),
                                   chatterl_mid_man:group_leave(ContentType,{Group,Client});
      "/groups/send/" ++ Group -> [Sender, Message] = get_properties(Req,["client","msg"]),
                                  chatterl_mid_man:group_send(ContentType,{Group,Sender,Message});
      "/groups/poll/" ++ Group -> chatterl_mid_man:group_poll(ContentType,Group);
      %% Authentication based queries.
      "/groups/create/" ++ Group ->
        [Description] = get_properties(Req,["description"]),
        case is_auth(Req) of
          {ok, _Msg} ->
            chatterl_mid_man:group_create(ContentType,{Group,Description});
          {error,Error} -> message_handler:get_response_body(ContentType,message_handler:build_carrier("error",Error))
        end;
      "/groups/drop/" ++ Group -> case is_auth(Req) of
                                    {ok,_Msg} ->
                                      chatterl_mid_man:group_drop(ContentType,Group);
                                    {error,Error} ->
                                      message_handler:get_response_body(ContentType,message_handler:build_carrier("error",Error))
                                  end;
      %% Catch all
      Unknown -> message_handler:get_response_body(ContentType,message_handler:build_carrier("error", "Unknown command: " ++Unknown))
    end,
  Req:respond({200, [{"Content-Type", ContentType}], list_to_binary(Response)}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Gets a list of properties.
%% @spec get_properties(Req,WantedParams) -> [ParamValues]
%%
%% @end
%%--------------------------------------------------------------------
get_properties(Req,WantedParams) ->
    Params = Req:parse_qs(),
    [proplists:get_value(Property, Params)|| Property <-  WantedParams].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Determines whether the client has successfully authenticated.
%%
%% Need to make this more secure.
%% @spec is_auth(Req) -> {ok,Msg}|{error,Error}
%%
%% @end
%%--------------------------------------------------------------------
is_auth(Req) ->
  case Req:get_header_value("authorization") of
    "Basic "++Base64 ->
      Str = base64:mime_decode_to_string(Base64),
      case string:tokens(Str, ":") of
        [User, Pass] ->
          chatterl_store:auth(User,Pass);
        _ ->
          {error,"Unauthorized authorization."}
      end;
    _ ->
      {error,"Need to authorize"}
  end.
