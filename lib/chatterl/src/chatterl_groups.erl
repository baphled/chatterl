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
-export([start/2,stop/0,name/0,description/0,list_groups/0,list_users/0]).

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
start(Name,Desc) ->
    case gen_server:start_link({global, ?SERVER}, ?MODULE, [Name,Desc], []) of
	{ok, Pid} ->
	    {ok, Pid};
	_ -> {error, "Group already exists"}
    end.
    

stop() ->
    gen_server:call({global, ?MODULE}, stop, infinity).

name() ->
    gen_server:call({global, ?SERVER}, name, infinity).
description() ->
    gen_server:call({global, ?SERVER}, description, infinity).

%% Calls to chatterl_serv
list_users() ->
    gen_server:call({global, 'chatterl_serv'}, list_users, infinity).
list_groups() ->
    gen_server:call({global, 'chatterl_serv'}, list_groups, infinity).
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
init([Name,Desc]) ->
    io:format("Initialising ~p...~n", [Name]),
    {ok, 
     #groups{
      users = gb_trees:empty(),
      name = Name,
      description = Desc}
    }.

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
    io:format("Shutting down...~n"),
    %Users = State#groups.users,
    Reply = drop_users(gb_trees:keys(State#groups.users), State#groups.users),
    {reply, Reply, State};
handle_call(name, _From, State) ->
    {reply, State#groups.name, State};
handle_call(description, _From, State) ->
    {reply, State#groups.description, State};
handle_call({update_users,Group}, _From, State) ->
    io:format("Updating users~n"),
    List = gb_trees:to_list(State#groups.users),
    NewTree = drop_user_from_group(State#groups.users,List,Group),
    {reply, ok, State#groups{users = NewTree} }.

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
    io:format("Shutting down Chatterl Groups...~n"),
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
%% @private
drop_user_from_group(UsersTree,[User|Users],Group) ->
    NewUsers = case gb_trees:lookup(User,UsersTree) of
  {value,Group} ->
      io:format("Dropped ~p from ~p~n",[User, Group]),
      gb_trees:delete(User, UsersTree);
        _ -> UsersTree      
    end,
    drop_user_from_group(NewUsers,Users,Group);
drop_user_from_group(UsersTree,[],_Group) ->
    UsersTree.

%% @private
drop_users([User|Users],UsersList) ->
    NewUsers = gb_trees:delete(User, UsersList),
    io:format("Delete: ~p~n", [User]),
    drop_users(Users, NewUsers);
drop_users([],UsersList) ->
    UsersList.

%% @private
user_exists(User,UsersTree) ->
    case gb_trees:is_defined(User, UsersTree) of
	true -> true;
	false -> false
    end.
