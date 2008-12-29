%%%-------------------------------------------------------------------
%%% File    : chatterl_groups_new.erl
%%% Author  : Yomi Akindayini <yomi@boodh.net>
%%% Description : Handle chatterl's group system
%%%
%%% Created : 18 Dec 2008 by Yomi Akindayini <yomi@boodah.net>
%%%-------------------------------------------------------------------
-module(chatterl_groups).
-behaviour(gen_server).

%% API
-export([start/2,stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-include("chatterl.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start(Name,Description) ->
    gen_server:start_link({global, Name}, ?MODULE, [Name,Description], []).

stop() ->
    Group = gen_server:call({global, ?MODULE}, name, infinity),
    gen_server:call({global, chatterl_client}, {drop_group, Group}, infinity),
    gen_server:call({global, ?MODULE}, stop, infinity).
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
init([Name,Description]) ->
    process_flag(trap_exit, true),
    io:format("Initialising ~p...~n", [Name]),
    {ok,
     #group{
       name = Name,
       description = Description,
       messages = gb_trees:empty(),
       users = gb_trees:empty()}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(name, _From, State) ->
    Result = State#group.name,
    Reply = {name, Result},
    {reply, Reply, State};
handle_call(description, _From, State) ->
    Result = State#group.description,
    Reply = {description, Result},
    {reply, Reply, State};
handle_call(list_users, _From, State) ->
    Reply = State#group.users,
    {reply, Reply, State};
handle_call({join, User}, From, State) ->
    {Reply, NewTree} =
	case gb_trees:is_defined(User, State#group.users) of
	    true ->
		io:format("~w joined~n", [User]),
		{{error, "Already joined"}, State};
	    false ->
		{{ok, "User added"}, gb_trees:insert(User, {User,From}, State#group.users)}
	end,
    {reply, Reply, State#group{ users=NewTree }};
handle_call({drop, User}, _From, State) ->
    {Reply, NewTree} =
	case gb_trees:is_defined(User, State#group.users) of
	    true ->
		io:format("~w disconnected~n", [User]),
		{{ok, dropped},
		 gb_trees:delete(User, State#group.users)};
	    false ->
		{{error, "Not connected"}, State}
	end,
    {reply, Reply, State#group{users=NewTree}};
handle_call({send_msg, User, Message}, _From, State) ->
    {Reply, NewTree} =
	case gb_trees:is_defined(Message, State#group.messages) of
	    false ->
		io:format("~p: ~p~n", [User, Message]),
		{{ok, msg_sent},
		gb_trees:insert(Message, {User, Message}, State#group.messages)};
	    true ->
		{{error, already_sent}, State}
	end,
    {reply, Reply, State#group{messages=NewTree}}.
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
    {unknown, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    io:format("Shutting down ~p~n", [State#group.name]),
    {shutdown, State#group.name}.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
%% @private
