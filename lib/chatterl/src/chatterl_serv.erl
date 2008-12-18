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
-export([start/0,stop/0,connect/1,create/2,drop/1,list_groups/0,group_exists/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-include("chatterl.hrl").

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

connect(User) ->
    gen_server:call({global, ?MODULE}, {connect,User}, infinity).

create(Group, Description) ->
    gen_server:call({global, ?MODULE}, {create, Group, Description}, infinity).

drop(Group) ->
    gen_server:call({global, ?MODULE}, {drop, Group}, infinity).

group_exists(Group) ->
    gen_server:call({global, ?MODULE}, {group_exists, Group}, infinity).

%% View the users connected to the server
list_groups() ->
    gen_server:call({global, ?MODULE}, list_groups, infinity).
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
    io:format("Initialising Chatterl Serv~n"),
    {ok, #chatterl{
       groups = gb_trees:empty(),
       users = gb_trees:empty()
       },
     #users{
       users = gb_trees:empty()
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
handle_call(list_groups, _Client, State) ->
    Result = gb_trees:keys(State#chatterl.groups),
    {reply, Result, State};
handle_call({connect,User}, _From, State) ->
    Reply = gb_trees:lookup(User, State#chatterl.users),
    {reply, Reply, State};
handle_call({create, Group, Description}, _From, State) ->
    NewTree =  case gb_trees:is_defined(Group, State#chatterl.groups) of
        true ->
		       Result = {error, "Group already created"},
		       State#chatterl.groups;
        false -> 
		       Result = {ok, Group},
		       gb_trees:insert(Group, {Group, Description}, State#chatterl.groups)
    end,
    {reply, Result, State#chatterl{ groups = NewTree }};

handle_call({drop, Group}, _From, State) ->
    NewTree =  case gb_trees:is_defined(Group, State#chatterl.groups) of
        true -> 
		       Result = {ok, "Group dropped"},
		       gb_trees:delete(Group, State#chatterl.groups);
        false -> 
		       Result = {error, "Unable to drop group."},
		       State#chatterl.groups
    end,
    {reply, Result, State#chatterl{ groups = NewTree }}.
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
