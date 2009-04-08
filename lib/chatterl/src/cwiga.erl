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
  [Path|Ext] = string:tokens(Req:get(path),"."),
  Method = Req:get(method),
  Post = Req:parse_post(),
  io:format("~p request for ~p with post: ~p~n", [Method, Path, Post]),
  Response = gen_server:call({global,?MODULE},{Method, Path, get_content_type(Ext), Post, Req}),
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
handle_call({'POST',Url,ContentType,Post,Req},_From,State) ->
  Reply = handle_response(handle_request('POST',Url,ContentType,Post,Req),ContentType),
  {reply, Reply, State};
handle_call({'GET',Url,ContentType,_Post,Req},_From,State) ->
  Reply = handle_response(handle_request('GET',Url,ContentType,Req),ContentType),
  {reply, Reply, State};
handle_call({'DELETE',Url,ContentType,_Post,Req},_From,State) ->
  Reply = handle_response(handle_request('DELETE',Url,ContentType,Req),ContentType),
  {reply, Reply, State};
handle_call({_,Url,ContentType,_Path,_Req},_From,State) ->
  Reply = send_response(error,{error("Unknown command: " ++Url, ContentType),ContentType}),
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
handle_request('GET', Url, ContentType, Req) ->
  Path = string:tokens(Url, "/"),
  case Path of
    ["users"] ->
      manage_request(ContentType,Req,{user_list,[]},false);
    ["users",Client,"connect"] ->
      chatterl_mid_man:connect(ContentType,Client);
    ["users",Group,"list"] ->
      manage_request(ContentType,Req,{user_list,Group},false);
    ["users",Client,"poll"] ->
      manage_request(ContentType,Req,{user_poll,Client},false);
    ["users",Client,"groups"] ->
      manage_request(ContentType,Req,{user_groups,Client},true);
    ["groups"] ->
      manage_request(ContentType,Req,{group_list,[]},false);
    ["groups",Group,"info"] ->
      manage_request(ContentType,Req,{group_info,Group},true);
    ["groups",Group,"poll"] ->
      manage_request(ContentType,Req,{group_poll,Group},false);
    ["status","logged_in"] ->
      manage_request(ContentType,Req,{logged_in,[]},true);
    _ -> error("Unknown command: " ++Url, ContentType)
  end;
handle_request('DELETE',Url,ContentType,Req) ->
  Path = string:tokens(Url, "/"),
  case Path of
  	["users",Client,"disconnect"] ->
    	chatterl_mid_man:disconnect(ContentType,Client);
		["groups",Group,"drop"] ->
      manage_request(ContentType,Req,{group_drop,Group},true);
    _ -> error("Unknown command: " ++Url, ContentType)
	end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Handles CWIGA's POST requests
%% @spec handle_request('POST', Url, ContentType, Path, Req) -> HTTPResponse
%%
%% @end
%%--------------------------------------------------------------------
handle_request('POST',Url,ContentType,Post,Req) ->
  Path = string:tokens(Url, "/"),
  case Path of
    ["users","register",Nick] ->
      chatterl_mid_man:register(ContentType,{Nick,get_params(["name","email","pass1","pass2"],Post)});
    ["users","login"] ->
      chatterl_mid_man:login(ContentType,get_params(["login","pass"],Post));
    ["users","logout"] ->
      chatterl_mid_man:logout(ContentType,proplists:get_value("client",Post));
    ["groups",Group,"send"] ->
      {Sender,Message} = get_params(["client","msg"],Post),
      manage_request(ContentType,Req,{group_send,{Group,Sender,Message}},false);
    ["users",Client,"send"] ->
      {Sender,Message} = get_params(["client","msg"],Post),
      manage_request(ContentType,Req,{user_msg,{Client,Sender,Message}},false);
    ["groups",Group,"join"] ->
      manage_request(ContentType,Req,{group_join,{Group,proplists:get_value("client",Post)}},false);
    ["groups",Group,"leave"] ->
      manage_request(ContentType,Req,{group_leave,{Group,proplists:get_value("client",Post)}},true);
    ["groups",Group,"create"] ->
      manage_request(ContentType,Req,{group_create,{Group,proplists:get_value("description",Post)}},true);
    _ -> error("Unknown command: " ++Url, ContentType)
  end.

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
%% @spec error(Response,ContentType) -> StatusCode
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
%% @spec manage_request(ContentType,Req,{Function,Args}) -> {ok,Msg}|{error,Error}
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
%% @spec is_auth(Req) -> {ok,Msg}|{error,Error}
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
