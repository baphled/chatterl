%%%-------------------------------------------------------------------
%%% @author Yomi Colledge <yomi@boodah.net>
%%% @doc
%%% Chatterl server process used to manipulate Chatterl
%%%
%%% This module is the main backend for Chatterl it managers who is
%%% connected, what groups are available and who is connected to them.
%%%
%%% In the near future this module will be refactored and renamed to
%%% allow for registered users to interact with directly or via the web.
%%% @end
%%% @copyright 2008 by Yomi Colledge <yomi@boodah.net>
%%%-------------------------------------------------------------------
-module(chatterl_serv).
-behaviour(gen_server).

%% API
-export([start/0,stop/0,connect/1,disconnect/1,create/2,drop/1]).
%% User specific
-export([list_users/0]).
%% Group specific
-export([group_description/1,list_groups/0,list_users/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(APP, "Chatterl").

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
    io:format("Starting ~s...~n",[?APP]),
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the server
%%
%% @spec stop() -> stopped
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:call({global, ?SERVER}, stop, infinity).

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
    gen_server:call({global,?MODULE},{create,Group,Description},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Drop a chatterl group from the server, this will send a message to
%% all users and the group to terminate related processes.
%%
%% @spec drop(Group) -> {ok,Message} | {error,Error}
%% @end
%%--------------------------------------------------------------------
drop(Group) ->
    gen_server:call({global,?MODULE},{drop,Group},infinity).

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
    io:format("Initialising ~s...~n",[?APP]),
    {ok, #chatterl{
       groups = gb_trees:empty(),
       users = gb_trees:empty()
       }}.

%%--------------------------------------------------------------------
%% @doc
%% Handling the call messages the chat server.
%%
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    io:format("Processing shut down ~s~n", [?APP]),
    {stop, normal, stopped, State};
handle_call(list_groups, _Client, State) ->
    {reply, gb_trees:keys(State#chatterl.groups), State};
handle_call({get_group, Group}, _From, State) ->
    Reply = case gb_trees:lookup(Group, State#chatterl.groups) of
		{value, Value} -> Value;
	        _ -> false
	    end,
    {reply, Reply, State};
handle_call({group_info,Group}, _From, State) ->
    Reply = [gen_server:call({global,Group},name),
	     gen_server:call({global,Group},description),
	     gen_server:call({global,Group},created)],
    {reply,Reply,State};
handle_call(list_users, _From, State) ->
    Reply = gb_trees:keys(State#chatterl.users),
    {reply, Reply, State};
handle_call({connect,User}, From, State) ->
    {Reply, NewTree} =
	case gb_trees:is_defined(User, State#chatterl.users) of
	    false->
		io:format("~s connected to ~s~n", [User,?MODULE]),
		{{ok, "connected"},
		 gb_trees:insert(User, {User,From}, State#chatterl.users)};
	    true -> {{error, "Unable to connect."},
		     State#chatterl.users}
	end,
    {reply, Reply, State#chatterl{ users = NewTree }};
handle_call({disconnect, User}, _From, State) ->
     {Reply,NewTree} =
	case gb_trees:is_defined(User, State#chatterl.users) of
	    true ->
		io:format("~s disconnecting...~n", [User]),
		{{ok, "User dropped"},
		 gb_trees:delete(User, State#chatterl.users)};
	    false ->
		{{error, "Unable to drop group."},
		 State#chatterl.users}
	end,
    {reply, Reply, State#chatterl{ users = NewTree }};
handle_call({disconnect, User, Groups}, _From, State) ->
    {Reply,NewTree} =
	case gb_trees:is_defined(User, State#chatterl.users) of
	    true ->
		lists:foreach(
		  fun(Group) ->
			  {Name,_JoinedOn,Pid} = Group,
			  io:format("~s disconnecting from ~s...~n", [User,Name]),
			  gen_server:call(Name, {leave, User}) end,
		  Groups),
		{{ok, "User dropped"}, gb_trees:delete(User, State#chatterl.users)};
	    false ->
		{{error, "Unable to drop user."},State#chatterl.users}
	end,
    {reply, Reply, State#chatterl{ users = NewTree }};
handle_call({create, Group, Description}, _From, State) ->
    {Reply, NewTree} =
	case gb_trees:is_defined(Group, State#chatterl.groups) of
	    true ->
		{{error, already_created},
		 State#chatterl.groups};
 	    false ->
		case chatterl_groups:start(Group, Description) of
		    {error, {Error,_Pid}} ->
			{{error, Error},
			 State#chatterl.groups};
		    {ok,GroupPid} ->
			link(GroupPid),
			io:format("Group created: ~s~n",[Group]),
			{{ok, GroupPid},
			 gb_trees:insert(Group, {Description, GroupPid}, State#chatterl.groups)}
		end
	end,
    {reply, Reply, State#chatterl{ groups=NewTree }};
handle_call({drop, Group}, _From, State) ->
    {Reply, NewTree} =
	case gb_trees:is_defined(Group, State#chatterl.groups) of
	    true ->
		io:format("Dropping group: ~s~n",[Group]),
		{value,{_Desc,Pid}} = gb_trees:lookup(Group, State#chatterl.groups),
		gen_server:call(Pid, stop),
		unlink(Pid),
		{{ok, "Group Dropped"},
		 gb_trees:delete(Group, State#chatterl.groups)};
	    false ->
		{{error, {"can not find",Group}},
		 State#chatterl.groups}
	end,
    {reply, Reply, State#chatterl{ groups = NewTree }};
handle_call({user_lookup, User}, _From, State) ->
    Reply =
	case gb_trees:is_defined(User, State#chatterl.users) of
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
    {reply, gb_trees:is_defined(User, State#chatterl.users), State};
handle_call({group_exists,Group}, _From, State) ->
    {reply, gb_trees:is_defined(Group, State#chatterl.groups), State}.

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
terminate(_Reason,State) ->
    case gb_trees:is_empty(State#chatterl.groups) of
	true ->
	    io:format("No groups to inform of shutdown~n");
	false ->
	    shutdown_groups(gb_trees:keys(State#chatterl.groups))
    end,
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
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determines whether a user exists on the server.
%%
%% @spec user_exists(User) -> bool
%% @end
%%--------------------------------------------------------------------
user_exists(User) ->
     gen_server:call({global, ?MODULE}, {user_exists, User}, infinity).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determines whether a group exists on chatterl.
%%
%% @spec group_exists(Group) -> bool
%% @end
%%--------------------------------------------------------------------
group_exists(Group) ->
    gen_server:call({global, ?MODULE}, {group_exists, Group}, infinity).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sends shutdown messages to all groups in the list.
%%
%% @spec shutdown_groups(GroupNam) -> void()
%% @end
%%--------------------------------------------------------------------
shutdown_groups(GroupNames) ->
    io:format("Shutting down groups~n"),
    lists:foreach(
      fun(GroupName) ->
	      io:format("Dropping group ~s...~n",[GroupName]),
	      gen_server:call({global,GroupName},stop,infinity)
      end,
      GroupNames).
