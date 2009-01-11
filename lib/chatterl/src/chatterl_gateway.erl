%%----------------------------------------------------------------
%%% @author  Yomi Colledge <yomi@boodah.net>
%%% @doc Web interface for Chatterl
%%%
%%% Chatterl Web Gateway
%%%
%%% Allows Chatterl to interface with any web-based interface
%%% Using JSON and XML, sending the requests off to the chatterl_serv
%%% module.
%%% @end
%%% @copyright 2008 Yomi Akindayini
%%%---------------------------------------------------------------
-module(chatterl_gateway).

-behaviour(gen_server).

-define(OK, <<"ok">>).
%% API
-export([start/1,dispatch_requests/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).
-record(messages, {name,message}).
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

dispatch_requests(Req) ->
  Path = Req:get(path),
  Action = clean_path(Path),
  handle(Action, Req).
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initiates the server
%%
%% Function: init(Args) -> {ok, State} |
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
handle_call(_Rquest, _From, State) ->
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
%% @doc
%%
%% Handles our RESTful resquests.
%% @spec handle(Action, Req) -> {ok, NewState}
%%
%% @end
%%--------------------------------------------------------------------
handle("/send", Req) ->
  Params = Req:parse_qs(),
  Sender = proplists:get_value("nick", Params),
  Group = proplists:get_value("to", Params),
  Message = proplists:get_value("msg", Params),
  chatterl_man:send_message(Sender, Group, Message),
  success(Req, ?OK);
%% Need to refactor so the request goes to chatterl_serv
handle("/connect/" ++ Client,Req) ->
    case chatterl_man:connect(Client) of
	ok ->
	    success(Req, ?OK);
	Error ->
	    error(Req, subst("Error: ~s", [Error]))
    end;
handle(_, Req) ->
  error(Req, "").

%%--------------------------------------------------------------------
%% @doc
%%
%% Convert a list into binary ready to be sent back to the web client.
%% @spec subst(Template,Values) -> binary()
%%
%% @end
%%--------------------------------------------------------------------
subst(Template, Values) when is_list(Values) ->
    list_to_binary(lists:flatten(io_lib:fwrite(Template, Values))).

%%--------------------------------------------------------------------
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
%% @doc
%%
%% Handles our error responses.
%%
%% Will eventually be JSON and XML based
%% @spec error(Req,Body) -> tuple()
%%
%% @end
%%--------------------------------------------------------------------
error(Req, Body) when is_binary(Body) ->
  Req:respond({500, [{"Content-Type", "text/plain"}], Body}).

%%--------------------------------------------------------------------
%% @doc
%%
%% Handles our successful responses.
%%
%% Will eventually be JSON and XML based
%% @spec success(Req,Body) -> tuple()
%%
%% @end
%%--------------------------------------------------------------------
success(Req, Body) when is_binary(Body) ->
  Req:respond({200, [{"Content-Type", "text/plain"}], Body}).

build_msg(Doc) ->
    [ {name, Doc#messages.name}, {msg, Doc#messages.message}].
 
to_json(Doc) ->
    mochijson:encode({struct, build_msg(Doc)}).
