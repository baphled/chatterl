%%%-------------------------------------------------------------------
%%% @author Yomi Colledge <yomi@boodah.net>
%%% @doc
%%% Handles the groups and client processes
%%% @end
%%% @copyright 2008 by Yomi Colledge <yomi@boodah.net>
%%%-------------------------------------------------------------------
-module(chatterl_serv).
-behaviour(gen_server).

%% API
-export([start/0,stop/0,connect/1,disconnect/1,create/2,drop/1,list_users/0]).
%% Group specific
-export([group_description/1,list_groups/0,group_exists/1,list_users/1]).
%% User specific
-export([user_exists/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-include("chatterl.hrl").

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
start() ->
    io:format("Starting chatterl server...~n"),
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the server
%%
%% @spec stop() -> stopped
%% @end
%%--------------------------------------------------------------------
stop() ->
    io:format("Stopping chatterl...~n"),
    gen_server:call({global, ?MODULE}, stop, infinity).

%%--------------------------------------------------------------------
%% @doc
%% Connects client to server, must be done before a user can interact with chatterl.
%%
%% @spec connect(User) -> {ok,Message} | {error,Error}
%% @end
%%--------------------------------------------------------------------
connect(User) ->
    gen_server:call({global, ?MODULE}, {connect,User}, infinity).

%%--------------------------------------------------------------------
%% @doc
%% Disconnect a client from the server, doing so will automatically disconnect
%% the client from the groups, they are logged into.
%%
%% @spec disconnect(User) -> {ok,Message} | {error,Error} 
%% @end
%%--------------------------------------------------------------------
disconnect(User) ->
    gen_server:call({global, ?MODULE}, {disconnect,User,[]}, infinity).

%%--------------------------------------------------------------------
%% @doc
%% Retrieves a group processes description
%%
%% @spec group_description(Group) -> {description,Description} | {error,Error} 
%% @end
%%--------------------------------------------------------------------
group_description(Group) ->
    case gen_server:call({global, ?MODULE}, {get_group, Group}, infinity) of
	{Name, GroupPid} -> 
	    case is_pid(GroupPid) of
		true -> gen_server:call(GroupPid, description);
		_ -> {error, {"Unable to find pid for ~",[Name]}}
	    end;
	_ -> {error, "Can not find group."}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Create a new chatterl group
%%
%% @spec create(Group,Description) -> {ok,Group} | {error,Error} 
%% @end
%%--------------------------------------------------------------------
create(Group, Description) ->
    case gen_server:call({global, ?MODULE}, {create, Group, Description}, infinity) of
	{ok, Group} ->
	    case chatterl_groups:start(Group, Description) of
		{error, Error} ->
		    {error, Error};
		{ok,GroupPid} -> 
		    gen_server:call({global, ?MODULE}, {add_pid, Group,GroupPid}, infinity),
		    link(GroupPid),
		    {ok, GroupPid}
	    end;
	_ -> io:format("Unable create group: ~p~n", [Group]),
	     {error, "Unable to create group"}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Drop a chatterl group from the server, this will send a message to
%% all users and the group to terminate related processes.
%%
%% @spec drop(Group) -> {ok,Message} | {error,Error} 
%% @end
%%--------------------------------------------------------------------
drop(Group) ->
    case group_exists(Group) of
	     true -> 
		 case gen_server:call({global, ?MODULE}, {get_group, Group}, infinity) of
		     {Group,Pid} ->
			 gen_server:call(Pid, stop),
			 gen_server:call({global, ?MODULE}, {remove_pid, Group}, infinity),
			 unlink(Pid),
			 {ok, "Group Dropped"};
		     {error, Error} ->
			 {error, Error}
		 end;
	     false -> {error, "Group not valid"}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Determines whether a user exists on the server.
%%
%% @spec user_exists(User) -> bool
%% @end
%%--------------------------------------------------------------------
user_exists(User) ->
     gen_server:call({global, ?MODULE}, {user_exists, User}, infinity).

%%--------------------------------------------------------------------
%% @doc
%% Determines whether a group exists on chatterl.
%%
%% @spec group_exists(Group) -> bool
%% @end
%%--------------------------------------------------------------------
group_exists(Group) ->
    gen_server:call({global, ?MODULE}, {group_exists, Group}, infinity).


%%--------------------------------------------------------------------
%% @doc
%% Lists all the users connected to chatterl
%%
%% @spec list_users() -> [Users] | [] 
%% @end
%%--------------------------------------------------------------------
list_users() ->
    gen_server:call({global, ?MODULE}, list_users, infinity).

%%--------------------------------------------------------------------
%% @doc
%% Lists all the users connected to a specific chatterl group
%%
%% @spec list_users(GroupName) -> [Users] | [] 
%% @end
%%--------------------------------------------------------------------
list_users(GroupName) ->
    case group_exists(GroupName) of
	true -> gen_server:call({global, GroupName}, list_users, infinity);
	false -> {error, "Group doesn't exist!"}
    end.

%%--------------------------------------------------------------------
%% @doc
%% List all the groups on chatterl.
%%
%% @spec list_groups() -> [Groups] | [] 
%% @end
%%--------------------------------------------------------------------
list_groups() ->
    gen_server:call({global, ?MODULE}, list_groups, infinity).
%%====================================================================
%% gen_server callbacks
%%====================================================================


%%--------------------------------------------------------------------
%% @doc
%% Initiates the server
%%
%% @spec init([]) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% @end 
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    io:format("Initialising Chatterl Serv~n"),
    {ok, #chatterl{
       groups = gb_trees:empty(),
       users = gb_trees:empty()
       }}.

%%--------------------------------------------------------------------
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(list_groups, _Client, State) ->
    Reply =  gb_trees:values(State#chatterl.groups),
    {reply, Reply, State};
handle_call({get_group, Group}, _From, State) ->
    Reply = case gb_trees:lookup(Group, State#chatterl.groups) of
		{value, Value} -> Value;
	        _ -> false
	    end,
    {reply, Reply, State};
handle_call(list_users, _From, State) ->
    Reply = gb_trees:keys(State#chatterl.users),
    {reply, Reply, State};
handle_call({connect,User}, From, State) ->
    {Reply, NewTree} = case gb_trees:is_defined(User, State#chatterl.users) of
			   false-> 
			       io:format("~p connected to ~p~n", [User,?MODULE]),
			       {{ok, "connected"},
			gb_trees:insert(User, {User,From}, State#chatterl.users)};
			   true -> {{error, "Unable to connect."},
				    State#chatterl.users}
		       end,
    {reply, Reply, State#chatterl{ users = NewTree }};
handle_call({disconnect, User}, _From, State) ->
     NewTree =  case gb_trees:is_defined(User, State#chatterl.users) of
        true -> 
			io:format("~p disconnecting...~n", [User]),
			Result = {ok, "User dropped"},
			gb_trees:delete(User, State#chatterl.users);
        false -> 
			Result = {error, "Unable to drop group."},
			State#chatterl.users
    end,
    {reply, Result, State#chatterl{ users = NewTree }};
handle_call({disconnect, User, Groups}, _From, State) ->
    {Reply,NewTree} =
	case gb_trees:is_defined(User, State#chatterl.users) of
	    true ->
		lists:foreach(fun(Group) ->
				      {_Name,Pid} = Group,
				      gen_server:call(Pid, {drop, User}, infinity) end, Groups),
		io:format("~p disconnecting...~n", [User]),
		{{ok, "User dropped"}, gb_trees:delete(User, State#chatterl.users)};
	    false -> 
		{{error, "Unable to drop group."},State#chatterl.users}
	end,
    {reply, Reply, State#chatterl{ users = NewTree }};
handle_call({create, Group, _Description}, _From, State) ->
    case gb_trees:is_defined(Group, State#chatterl.groups) of
        true ->
	    Result = {error, "Group already created"};
	false -> 
	    Result = {ok, Group}
    end,
    {reply, Result, State};
handle_call({add_pid, Group, GroupPid}, _From, State) ->
    {Reply,NewTree} = 
	case gb_trees:is_defined(Group, State#chatterl.groups) of
	    true ->
		{{error, "Groups already created"},
		 State#chatterl.groups};
	    false ->
		{{ok, "Created group..."},
		 gb_trees:insert(Group, {Group, GroupPid}, State#chatterl.groups)}
	end,
    {reply, Reply, State#chatterl{ groups = NewTree }};
handle_call({remove_pid, Group}, _From, State) ->
    {Reply, NewTree} = case gb_trees:is_defined(Group, State#chatterl.groups) of
	true ->
			{gb_trees:lookup(Group, State#chatterl.groups),
			gb_trees:delete(Group, State#chatterl.groups)};
        false ->
			{{error, {"can not find",Group}},
			State#chatterl.groups}
    end,
    {reply, Reply, State#chatterl{ groups = NewTree }};
handle_call({user_lookup, User}, _From, State) ->
    Reply = case gb_trees:is_defined(User, State#chatterl.users) of
		 true ->
		     case gb_trees:lookup(User, State#chatterl.users) of
			 {value, {UserName, {UserPid, _UserPidRef}}} ->
			     {ok,UserName,UserPid};
			 _ ->
			     {error, "Unable to lookup user"}
		     end;
		 false ->
		     {error, "Cannot find user!"}
	     end,
    {reply, Reply, State};
handle_call({user_exists, User}, _From, State) ->
    Reply = gb_trees:is_defined(User, State#chatterl.users),
    {reply, Reply, State};
handle_call({group_exists,Group}, _From, State) ->
    Reply = gb_trees:is_defined(Group, State#chatterl.groups),
    {reply, Reply, State};
handle_call({join, Group, User}, _From, State) ->
    Reply = chatterl_serv:join(Group,User),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @doc
%% Handling cast message
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
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% Terminates chatterl_serv
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(normal, _State) ->
    %% Here we need to loop through each of our processes send a stop message
    %% First well drop all groups, then we'll drop users.
    ok;
terminate(_Reason,_State) ->
    ok.
%%--------------------------------------------------------------------
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


