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
-export([start/1,stop/0,get_params/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(APP, "CWIGA").
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
start(Port) ->
  gen_server:start_link({global, ?SERVER}, ?MODULE, [Port], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the server
%%
%% @spec stop() -> {ok,Pid} | stopped | {error,Error}
%% @end
%%--------------------------------------------------------------------
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
  [Url|Ext] = string:tokens(Req:get(path),"."),
  Method = Req:get(method),
  Post = Req:parse_post(),
  io:format("~p request for ~p with post: ~p~n", [Method, Url, Post]),
	Path = string:tokens(Url, "/"),
  Response = gen_server:call({global,?MODULE},{Method, Path, get_content_type(Ext), Post, Req}),
  Req:respond(Response).

get_path(Req) ->
	[Path|_Ext] = string:tokens(Req:get(path),"."),
	Path.
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
handle_call({'POST',Path,ContentType,Post,Req},_From,State) ->
  Reply = handle_response(handle_request('POST',Path,ContentType,Post,Req),ContentType),
  {reply, Reply, State};
handle_call({'GET',Path,ContentType,_Post,Req},_From,State) ->
  Reply = handle_response(handle_request('GET',Path,ContentType,Req),ContentType),
  {reply, Reply, State};
handle_call({'DELETE',Path,ContentType,_Post,Req},_From,State) ->
  Reply = handle_response(handle_request('DELETE',Path,ContentType,Req),ContentType),
  {reply, Reply, State};
handle_call({_,_Path,ContentType,_Path,Req},_From,State) ->
  Reply = send_response(error,{error("Unknown command: " ++ get_path(Req), ContentType),ContentType}),
  {reply, Reply, State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

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
%% Handles CWIGA's GET requests
%% @spec handle_request('GET', Url, ContentType, Req) -> HTTPResponse
%%
%% @end
%%--------------------------------------------------------------------
handle_request('GET',["users"],ContentType,_Req) ->
	manage_request(ContentType,Req,{user_list,[]},false);
handle_request('GET',["groups"],ContentType,_Req) ->
	manage_request(ContentType,Req,{group_list,[]},false);
handle_request('GET',["users",Client,"connect"],ContentType,_Req) ->
	chatterl_mid_man:connect(ContentType,Client);
handle_request('GET',["users",Group,"users"],ContentType,_Req) ->
	manage_request(ContentType,Req,{user_list,Group},false);
handle_request('GET',["users",Client,"groups"],ContentType,_Req) ->
	manage_request(ContentType,Req,{user_groups,Client},true);
handle_request('GET',["users",Client,"poll"],ContentType,_Req) ->
	manage_request(ContentType,Req,{user_poll,Client},false);
handle_request('GET',["groups",Group,"info"],ContentType,_Req) ->
	manage_request(ContentType,Req,{group_info,Group},false);
handle_request('GET',["groups",Group,"poll"],ContentType,_Req) ->
	manage_request(ContentType,Req,{group_poll,Group},false);
handle_request('GET',["status","logged_in"],ContentType,_Req) ->
	manage_request(ContentType,Req,{logged_in,[]},true);
handle_request('GET', _Path, ContentType, Req) ->
	error("Unknown command: " ++ get_path(Req), ContentType);
handle_request('DELETE',["users",Client,"disconnect"],ContentType,_Req) ->
  chatterl_mid_man:disconnect(ContentType,Client);
handle_request('DELETE',["groups",Group,"drop"],ContentType,_Req) ->
	manage_request(ContentType,Req,{group_drop,Group},true);
handle_request('DELETE',_Path,ContentType,Req) ->
	error("Unknown command: " ++ get_path(Req), ContentType).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Handles CWIGA's POST requests
%% @spec handle_request('POST', Url, ContentType, Path, Req) -> HTTPResponse
%%
%% @end
%%--------------------------------------------------------------------
handle_request('POST',["users","register",Nick],ContentType,Post,Req) ->
	chatterl_mid_man:register(ContentType,{Nick,get_params(["name","email","pass1","pass2"],Post)});
handle_request('POST',["users","login"],ContentType,Post,Req) ->
	chatterl_mid_man:login(ContentType,get_params(["login","pass"],Post));
handle_request('POST',["users","logout"],ContentType,Post,Req) ->
	chatterl_mid_man:logout(ContentType,proplists:get_value("client",Post));
handle_request('POST',["groups",Group,"send"],ContentType,Post,Req) ->	
  {Sender,Message} = get_params(["client","msg"],Post),
	manage_request(ContentType,Req,{group_send,{Group,Sender,Message}},false);
handle_request('POST',["users",Client,"send"],ContentType,Post,Req) ->
	{Sender,Message} = get_params(["client","msg"],Post),
  manage_request(ContentType,Req,{user_msg,{Client,Sender,Message}},false);
handle_request('POST',["groups",Group,"join"],ContentType,Post,Req) ->
	manage_request(ContentType,Req,{group_join,{Group,proplists:get_value("client",Post)}},false);
handle_request('POST',["groups",Group,"leave"],ContentType,Post,Req) ->
	manage_request(ContentType,Req,{group_leave,{Group,proplists:get_value("client",Post)}},true);
handle_request('POST',["groups",Group,"create"],ContentType,Post,Req) ->
	manage_request(ContentType,Req,{group_create,{Group,proplists:get_value("description",Post)}},true);
handle_request('POST',_Path,ContentType,Post,Req) ->
	error("Unknown command: " ++ get_path(Req), ContentType).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Gets our porameters
%%
%% Takes a list of parameters keys used to retrieve their values in the
%% same order.
%% @spec get_params(Params,Post) -> Parameters
%%
%% @end
%%--------------------------------------------------------------------
get_params(Params,Post) ->
	list_to_tuple([proplists:get_value(Param,Post) || Param <- Params]).
		
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
    ["json"] -> ["text/json"];
    ["xml"] -> ["text/xml"];
    _ -> ["text/json"]
 end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Wrapper method used for successful responses
%%
%% @spec check_json_response(Json) -> JSONResponse
%% @end
%%--------------------------------------------------------------------
check_json_response(Json) ->
  {struct,[{<<"chatterl">>,
            {struct,[{<<"response">>,{struct,[Response]}}]}}]} =
    mochijson2:decode(Json),
  Response.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Wrapper method used for error responses
%%
%% @spec get_status_code(ResponseType) -> StatusCode
%% @end
%%--------------------------------------------------------------------
get_status_code(ResponseType) ->
  case ResponseType of
    <<"success">> -> 200;
    <<"unauth">> -> 401;
    <<"error">> -> 404;
    <<"failure">> -> 500
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Used for generic responses
%%
%% @spec response_message(Type,Message,ContentType) -> HTTPResponse
%% @end
%%--------------------------------------------------------------------
response_message(Type,Message,ContentType) ->
  message_handler:get_response_body(ContentType,
                                    message_handler:build_carrier(Type, Message)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Wrapper method used for error responses
%%
%% @spec error(Message,ContentType) -> HTTPResponse
%% @end
%%--------------------------------------------------------------------
error(Message,ContentType) ->
  response_message("error", Message,ContentType).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Wrapper method used for error responses
%%
%% @spec send_response(ResponseType,{Response,ContentType}) -> HTTPResponse
%% @end
%%--------------------------------------------------------------------
send_response(ResponseType,{Response,ContentType}) ->
  StatusCode = get_status_code(ResponseType),
  {StatusCode, [{"Content-Type", ContentType}], list_to_binary(Response)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sets up the needed response type for our results.
%%
%% @spec handle_response(Response,ContentType) -> HTTPResponse
%% @end
%%--------------------------------------------------------------------
handle_response(Response,ContentType) ->
  case ContentType of
    ["text/xml"] ->
      send_response(<<"success">>,{Response,ContentType});
    ["text/json"] ->
      case check_json_response(Response) of
        {ResponseType,_} -> send_response(ResponseType,{Response,ContentType})
      end
  end.

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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Manages the calls that need to be passed to manage_auth
%%
%% @spec manage_request(ContentType,Req,{Function,Args}) -> 
%%																							{ok,Msg} | {error,Error}
%%
%% @end
%%--------------------------------------------------------------------
manage_request(ContentType,Req,{Function,Args},Auth) ->
  Parameters =
    case Args of
      [] ->
        Fun = fun(CT) -> apply(chatterl_mid_man,Function,[CT]) end,
        ContentType;
      Params ->
        Fun = fun({CT,Arg}) -> apply(chatterl_mid_man,Function,[CT,Arg]) end,
        {ContentType,Params}
    end,
  manage_auth(Auth,{ContentType,Req,{Fun,Parameters}}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Manages all our authorisational based calls
%%
%% @spec manage_auth(Auth,{ContentType,Req,{Fun,Parameters}}) ->
%%																							{ok,Msg}|{error,Error}
%%
%% @end
%%--------------------------------------------------------------------
manage_auth(Auth,{ContentType,Req,{Fun,Parameters}}) ->
  case Auth of
    true ->
      authorise(ContentType,Req,{Fun,Parameters});
    false ->
      Fun(Parameters)
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Authorises a client and then carries out an action.
%%
%% Basic wrapper used to pass parameters to an anonymouse function.
%% @spec authorise(ContentType,Req,{Fun,Params}) -> {ok,Msg}|{error,Error}
%%
%% @end
%%--------------------------------------------------------------------
authorise(ContentType,Req,{Fun,Params}) ->
  case is_auth(Req) of
    {error,Error} ->
      response_message('unauth',Error,ContentType);
    {ok,_} ->
      Fun(Params)
  end.
