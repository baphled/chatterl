%%----------------------------------------------------------------
%%% @author  Yomi Colledge <yomi@boodah.net>
%%% @doc Web interface for Chatterl
%%%
%%% Chatterl Administration web interface
%%%
%%% Allows Chatterl admins to manipulate Chatterl.
%%%
%%% Eventually this will also handle adding plug-ins to the system
%%% and activating various modules (chat bots, FB Connect, etc).
%%%
%%% This will be seperate from the actual REST web server which
%%% will be used primarily for clients only.
%%% @end
%%% @copyright 2008 Yomi Akindayini
%%%---------------------------------------------------------------
-module(chatterl_web).

-behaviour(gen_server).

-define(CONTENT, <<"<html><head><title>Hello</title></head><body>Welcome to <a href='/admin/'>Chatterl Admin</a></body></html>">>).

%% API
-export([start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).
-define(SERVER, ?MODULE).
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start() -> {ok,Pid} | ignore | {error,Error}
%% @end
%%--------------------------------------------------------------------
start(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State};
handle_call({Unknown, Req}, _From, State) ->
    Req:response({404, [{"Content-Type", "text/plain"}],
		 subst("Unknown action: ~s", [Unknown])}),
    {reply,Req,State}.
    

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
dispatch_requests(Req) ->
    Path = Req:get(path),
    Action = clean_path(Path),
    gen_server:call(?MODULE,{Action, Req}, infinity).

subst(Template, Values) when is_list(Values) ->
    list_to_binary(lists:flatten(io_lib:fwrite(Template, Values))).

clean_path(Path) ->
    case string:str(Path, "?") of
	0 ->
	    Path;
	N ->
	    string:substr(Path, 1, string:len(Path) - (N + 1))
    end.
