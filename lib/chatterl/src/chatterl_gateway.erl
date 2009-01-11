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
-export([start/1,dispatch_requests/1,xml_to_list/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).
-record(messages, {client,message}).
%% Record used to pass our basic messages from the gateway to chatterl_web
-record(carrier, {type,message}).
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
  success(Req, {"text/plain",?OK});
%% Need to refactor so the request goes to chatterl_serv
handle("/connect/" ++ Client,Req) ->
    case gen_server:call({global,chatterl_serv},{connect,Client}) of
	{ok,_} ->
	    Response = to_json(#carrier{ type=success, message=Client ++ " now connected"}),
	    success(Req, {"text/json",Response});
	{error,Error} ->
	    Response = to_json(#carrier{ type=fail, message=Error}),
	    success(Req, {"text/json",Response})
    end;
handle("/", Req) ->
    Response = to_json(#carrier{ type=error, message="Illegal method"}),
    error(Req,{"text/json",Response}).

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
error(Req, {ContentType,Body}) when is_list(Body) ->
  Req:respond({500, [{"Content-Type", ContentType}], list_to_binary(Body)}).

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
success(Req, {ContentType,Body}) when is_list(Body) ->
  Req:respond({200, [{"Content-Type", ContentType}], list_to_binary(Body)}).

to_json(Doc) ->
    case Doc of
	{carrier, Type, Message} ->
	    mochijson:encode({array, [Type,Message]});
	_ -> io:format("Illegal message~n")
    end.

%% Generic message
xml_message(Record) ->
    case Record of
	{carrier, Type, Message} ->
	    xml_to_list(xml_tuple(list_to_atom(Type),Message),[]);
	_ -> io:format("Unmatched record.~n")
    end.

xml_tuple(Type,Message) ->
    {chatterl, [], [message,[],[{Type,[Message]}]]}.

xml_to_list(Xml,Prolog) ->
  lists:flatten(xmerl:export_simple([Xml],xmerl_xml,[{prolog,Prolog}])).
