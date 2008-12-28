%%%-------------------------------------------------------------------
%%% File    : chatterl_client.erl
%%% Author  : Yomi Colledge <yomi@boodah.net>
%%% Description : Basic client for chat system
%%%
%%% Created : 27 Dec 2008 by Yomi Colledge <yomi@boodah.net>
%%%-------------------------------------------------------------------
-module(chatterl_client).

-behaviour(gen_server).

%% API
-export([start/1,stop/0,join/1,name/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(user, {name, ip}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start(User) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [User], []).

stop() ->
    gen_server:call({local, ?MODULE}, stop, infinity).

name() ->
    gen_server:call(chatterl_client, client_name, infinity).

join(Group) ->
    case gen_server:call({global, chatterl_serv}, {get_group, Group}, infinity) of
	{error, Error} ->
	    {error, Error};
	{_GroupName,GroupPid} ->
	    case gen_server:call(chatterl_client, client_name, infinity) of
		{name, Name} -> gen_server:call(GroupPid, {join, Name}, infinity);
		_ -> {error, "Unable to connect"}
	    end;
	false ->
	    {error, "Group doesn't exist"}
    end.
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
init([User]) ->
    process_flag(trap_exit, true),
    case gen_server:call({global, chatterl_serv}, {connect, User}, infinity) of
	{error, Error} ->
	    io:format("~p~n", [Error]),
	    {stop, Error};
	{ok, Message} ->
	    io:format("~p is ~p.~n", [User, Message]),
	    {ok, #user{name = User}}
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(client_name, _From, State) ->
    Reply = {name, State#user.name},
    {reply, Reply, State};
handle_call({join, Group}, _From, State) ->
    Reply = chatterl_serv:join(Group, State#user.name),
    {reply, Reply, State}.

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
terminate(_Reason, State) ->
    gen_server:call({global, chatterl_serv}, {disconnect,State#user.name}, infinity),
    io:format("~p is disconnecting...", [State#user.name]),
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


