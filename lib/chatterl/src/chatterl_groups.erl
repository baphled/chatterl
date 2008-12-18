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
-export([start/0,stop/0,list_groups/0,list_users/0,join_group/2,leave_group/1,create/2,drop/1]).

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
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call({global, ?MODULE}, stop, infinity).

list_users() ->
    gen_server:call({global, ?MODULE}, list_users, infinity).

list_groups() ->
    gen_server:call({global, 'chatterl_serv'}, list_groups, infinity).
join_group(User,Group) ->
    gen_server:call({global, ?MODULE}, {join_group, User, Group}, infinity).

leave_group(User) ->
    gen_server:call({global, ?MODULE}, {leave_group, User}, infinity).

create(Group,Desc) ->
    Message = case gen_server:call({global, 'chatterl_serv'}, {create, Group, Desc}) of
	{ok, GroupName} ->
	    "Created group: "++GroupName;
	{error, Error} ->
	    "Error: " ++ Error
     end,
    {ok, Message}.

drop(Group) ->
    io:format("dropping ~p group...~n", [Group]),
    case gen_server:call({global, 'chatterl_serv'}, {drop, Group}) of
	{ok, _Result} -> gen_server:call({global, ?MODULE}, {update_users, Group});
        {error, Error} -> io:format("Error:~p~n", [Error])
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
init([]) ->
    io:format("Initialising ~p...~n", [?MODULE]),
    {ok, #groups{users = gb_trees:empty()}}.

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
handle_call(list_users, _From, State) ->
    Reply = gb_trees:keys(State#groups.users),
    {reply, Reply, State};

handle_call({leave_group, User}, _From, State) ->
    NewUsers = case user_exists(User, State#groups.users) of
	true ->
		       Reply = "Disconnected: ",
		       gb_trees:delete(User, State#groups.users);
	false -> 
		       Reply = "is not connected.",
		       State#groups.users
    end,
    {reply, Reply, State#groups{ users = NewUsers }};
handle_call({join_group, User, Group}, _From, State) ->
    NewTree = case chatterl_serv:group_exists(Group) of
	true -> 
	    case user_exists(User, State#groups.users) of
		true -> 
		    io:format("~p already connected to a group, ~p~n", [User, Group]), 
		    State#groups.users;
		
		false ->
		    io:format("~p connected to: ~p~n", [User, Group]),
		    gb_trees:insert(User, {User,Group}, State#groups.users)
	    end;
	false ->
	    io:format("~p, group doesn't exists: ~p~n", [User, Group]),
	    State#groups.users;
	{error, Error} ->
	    {error, Error}
    end,
    {reply, ok, State#groups{ users = NewTree }};
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