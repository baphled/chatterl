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
-export([start/0,stop/0]).

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
start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call({local, ?MODULE}, stop, infinity).

create(Group, Description) ->
    gen_server:call({global, ?MODULE}, {create, Group, Description}, stop, infinity).
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
init(_) ->
    process_flag(trap_exit, true),
    io:format("Initialising chatterl group handler...~n"),
    {ok,
     #group{
      users = gb_trees:empty(),
      rooms = gb_trees:empty()}}.

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
    Reply = drop_users(gb_trees:keys(State#group.users), State#group.users),
    {reply, Reply, State};
handle_call({create, Group, Description}, _From, State) ->
    {Reply, NewTree} =
	case gb_trees:is_defined(Group, State#group.rooms) of
	    true -> {{already_exists, Group},
		     State#group.rooms};
	    false -> {ok, gb_trees:insert(Group, {Group, Description}, State#group.rooms)}
	end,
    {reply, Reply, State#group{rooms = NewTree}};
handle_call(list_groups, _From, State) ->
    {reply, State#group.rooms, State};
handle_call({update_users,Group}, _From, State) ->
    io:format("Updating users~n"),
    List = gb_trees:to_list(State#group.users),
    NewTree = drop_user_from_group(State#group.users,List,Group),
    {reply, ok, State#group{users = NewTree} }.

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
terminate(normal, _State) ->
    io:format("Shutting down Chatterl Group...~n"),
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
