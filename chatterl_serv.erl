%%%-------------------------------------------------------------------
%%% File    : chatterl_serv.erl
%%% Author  : Yomi Akindayini <yomi@boodah.net>
%%% Description : Chatterl server, used to manage users.
%%%
%%% Created : 13 Dec 2008 by Yomi Akindayini <yomi@boodah.net>
%%%-------------------------------------------------------------------
-module(chatterl_serv).

-behaviour(gen_server).

%% API
-export([start/0,stop/0,create/2,drop/1,call/2,call/3,view_groups/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(chatterl, {groups, lastcall}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start() ->
    io:format("Starting chatterl...~n"),
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call({global, ?MODULE}, stop, infinity).

create(GroupPid, Name) ->
    gen_server:call({global, ?MODULE}, {create, GroupPid, Name}, infinity).

drop(GroupPid) ->
    gen_server:call({global, ?MODULE}, {drop, GroupPid}, infinity).

%% Used to make general calls to the server.
call(Client,Method) ->
    twitterl:call(Client, Method, []).
call(Client, Method, Args) ->
    gen_server:call({global, ?MODULE}, {Client, Method, Args}, infinity).

%% View the users connected to the server
view_groups() ->
    gen_server:call({global, ?MODULE}, view_groups, infinity).
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    io:format("Initialising chatterl~n"),
    {ok, #chatterl{
       groups = gb_trees:empty(),
       lastcall = calendar:datetime_to_gregorian_seconds(erlang:universaltime())
      }}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _Client, State) ->
    {stop, normal, stopped, State};
handle_call(view_groups, _Client, State) ->
    Result = gb_trees:keys(State#chatterl.groups),
    {reply, Result, State};

handle_call({create, GroupPid, Name}, _From, State) ->
    NewTree =  case gb_trees:is_defined(GroupPid, State#chatterl.groups) of
        true ->
		       Result = {error, "Group already created"},
		       State#chatterl.groups;
        false -> 
		       Result = {ok, Name},
		       gb_trees:insert(GroupPid, {GroupPid, Name}, State#chatterl.groups)
    end,
    {reply, Result, State#chatterl{ groups = NewTree }};

handle_call({drop, GroupPid}, _From, State) ->
    NewTree =  case gb_trees:is_defined(GroupPid, State#chatterl.groups) of
        true -> 
		       Result = "Session dropped",
		       gb_trees:delete(GroupPid, State#chatterl.groups);
        false -> 
		       Result = "Unable to drop session.",
		       State#chatterl.groups
    end,
    {reply, Result, State#chatterl{ groups = NewTree }};
handle_call({Client, Method, Args}, _From, State) ->
    Now = calendar:datetime_to_gregorian_seconds(erlang:universaltime()),
    Response = case group_exists(State, Client) of
        {error, Reason} -> {error, Reason};
        {GroupPid, Name} ->
            try apply(chatterl_serv, Method, [GroupPid, Name, Args])
            catch
                Err:Msg ->
                    io:format("~p:~p~n", [Err, Msg]),
                    {error, {Method, Args}}
            end;
        _ -> {error, unknown}
    end,
    {reply, Response, State#chatterl{ lastcall = Now }}.
%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
group_exists(State, Client) ->
    case gb_trees:is_defined(Client, State#chatterl.groups) of
        false -> {error, {invalid_client, Client}};
        true -> gb_trees:get(Client, State#chatterl.groups)
    end.

