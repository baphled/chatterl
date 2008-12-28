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
%% Function: start() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start() ->
    io:format("Starting chatterl server...~n"),
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

stop() ->
    io:format("Stopping chatterl...~n"),
    gen_server:call({global, ?MODULE}, stop, infinity).

connect(User) ->
    gen_server:call({global, ?MODULE}, {connect,User}, infinity).

disconnect(User) ->
    gen_server:call({global, ?MODULE}, {disconnect,User}, infinity).

group_description(Group) ->
    case gen_server:call({global, ?MODULE}, {get_group, Group}, infinity) of
	{Name, GroupPid} -> 
	    case is_pid(GroupPid) of
		true -> gen_server:call(GroupPid, description);
		_ -> {error, {"Unable to find pid for ~",[Name]}}
	    end;
	_ -> {error, "Can not find group."}
    end.

create(Group, Description) ->
    case gen_server:call({global, ?MODULE}, {create, Group, Description}, infinity) of
	{ok, Group} ->
	    case chatterl_groups:start(Group, Description) of
		{error, Error} ->
		    {error, Error};
		{ok,GroupPid} -> 
		    gen_server:call({global, ?MODULE}, {add_pid, Group,GroupPid}, infinity),
		    link(GroupPid)
	    end;
	_ -> io:format("Unable create group: ~p~n", [Group])
    end.

drop(Group) ->
    case group_exists(Group) of
	     true -> 
		 case gen_server:call({global, ?MODULE}, {get_group, Group}, infinity) of
		     {Group,Pid} ->
			 gen_server:call(Pid, stop),
			 gen_server:call({global, ?MODULE}, {remove_pid, Group}, infinity),
			 unlink(Pid);
		     {error, Error} ->
			 {error, Error}
		 end;
	     false -> {error, "Group not valid"}
    end.
	    
user_exists(User) ->
     gen_server:call({global, ?MODULE}, {user_exists, User}, infinity).

group_exists(Group) ->
    gen_server:call({global, ?MODULE}, {group_exists, Group}, infinity).

%% View the users connected to the server
list_users() ->
    gen_server:call({global, ?MODULE}, list_users, infinity).

list_users(Group) ->
    case group_exists(Group) of
	true -> gen_server:call({global, Group}, list_users, infinity);
	false -> {error, "Group doesn't exist!"}
    end.

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
    process_flag(trap_exit, true),
    io:format("Initialising Chatterl Serv~n"),
    {ok, #chatterl{
       groups = gb_trees:empty(),
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
		false-> {{ok, "connected"},
			gb_trees:insert(User, {User,From}, State#chatterl.users)};
		true -> {{error, "Unable to connect."},
			 State#chatterl.users}
	    end,
    {reply, Reply, State#chatterl{ users = NewTree }};
handle_call({disconnect, User}, _From, State) ->
     NewTree =  case gb_trees:is_defined(User, State#chatterl.users) of
        true -> 
		       Result = {ok, "User dropped"},
		       gb_trees:delete(User, State#chatterl.users);
        false -> 
		       Result = {error, "Unable to drop group."},
		       State#chatterl.users
    end,
    {reply, Result, State#chatterl{ users = NewTree }};
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
		case erlang:is_process_alive(GroupPid) of
		    true -> 
			{{ok, "Created group..."},
			     gb_trees:insert(Group, {Group, GroupPid}, State#chatterl.groups)};
		    false -> {{error, "Unable to add group"},
			      State#chatterl.groups}
		end
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
			 {value, {_UserName, UserPid, _UserPidRef}} ->
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
    %% Here we need to loop through each of our processes send a stop message
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


