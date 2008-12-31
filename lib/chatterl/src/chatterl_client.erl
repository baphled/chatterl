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
-export([start/1,stop/0,join/1,drop/1,name/0,connected_to/0,list_groups/0]).
-export([private_msg/2,send_msg/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(user, {name, ip, groups}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @edoc
%% Starts the server
%%
%% @spec start(User) -> {ok,Pid} | ignore | {error,Error} 
%% @end
%%--------------------------------------------------------------------
start(User) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [User], []).

%%--------------------------------------------------------------------
%% @edoc
%% Stops the server
%%
%% @spec stop() -> stopped
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:call(?MODULE, stop, infinity).

%%--------------------------------------------------------------------
%% @edoc
%% Gets the clients name
%%
%% @spec name() -> {name,Name} | {error,Error}
%% @end
%%--------------------------------------------------------------------
name() ->
    gen_server:call(chatterl_client, client_name, infinity).

%%--------------------------------------------------------------------
%% @edoc
%% Retrieves a list of groups the client is connected to
%%
%% @spec connected_to() -> [Groups] | []
%% @end
%%--------------------------------------------------------------------
connected_to() ->
    gen_server:call(chatterl_client, groups, infinity).

%%--------------------------------------------------------------------
%% @edoc
%% Retrieves a list of avaliable groups
%%
%% @spec list_groups() -> [Groups] | []
%% @end
%%--------------------------------------------------------------------
list_groups() ->
    gen_server:call({global, chatterl_serv}, list_groups, infinity).

%%--------------------------------------------------------------------
%% @edoc
%% Allows a client to join a group
%%
%% @spec join(Group) -> {ok,Msg} | {error,Error}
%% @end
%%--------------------------------------------------------------------
join(Group) ->
    case gen_server:call({global, chatterl_serv}, {get_group, Group}, infinity) of
	{error, Error} ->
	    {error, Error};
	{GroupName,GroupPid} ->
	    case gen_server:call(chatterl_client, client_name, infinity) of
		{name, Name} -> 
		    case gen_server:call(GroupPid, {join, Name}, infinity) of
			{ok, Msg} ->
			    gen_server:call(chatterl_client, {add_group, GroupName, GroupPid}, infinity),
			    {ok, Msg};
			_ -> {error, "Unable to connect!"}
		    end;
		_ -> 
		    {error, "Unable to connect"}
	    end;
	false ->
	    {error, "Group doesn't exist"}
    end.

%%--------------------------------------------------------------------
%% @edoc
%% Allows the uesr to drop their selves from a group
%%
%% @spec drop(Group) -> {ok,Msg} | {error,Error}
%% @end
%%--------------------------------------------------------------------
drop(Group) ->
    case gen_server:call({global, chatterl_serv}, {get_group, Group}, infinity) of
	{GroupName, GroupPid} ->
	    case gen_server:call(chatterl_client, client_name, infinity) of
		{name, Name} ->
		    case gen_server:call(GroupPid, {drop, Name}, infinity) of
			{ok, Msg} ->
			    gen_server:call(chatterl_client, {drop_group, GroupName}, infinity),
			    {ok, Msg};
			{error, Error} -> {error, Error}
		    end;
		_ ->
		    {error, "Unable to disconnect!"}
	    end;
	false ->
	    {error, "Group doesn't exist"}
    end.

%%--------------------------------------------------------------------
%% @edoc
%% Sends a message to a group
%%
%% @spec send_msg(Group,Msg) -> {ok,msg_sent} | {error,Error}
%% @end
%%--------------------------------------------------------------------
send_msg(Group,Msg) ->
    case gen_server:call({global, chatterl_serv}, {get_group, Group}, infinity) of
	false ->
	    {error, "Unable to send message"};
	{_GroupName, GroupPid} ->
	    {name,User} = gen_server:call(chatterl_client, client_name, infinity),
	    case gen_server:call(GroupPid, {send_msg, User, Msg}, infinity) of
		{ok, msg_sent} ->
		    io:format("Sent message~n");
		{error, Error} ->
		    {error, Error}
	    end
    end.

%%--------------------------------------------------------------------
%% @edoc
%% Sends a private message to a connected client.
%%
%% @spec private_msg(User,Msg) -> {ok,Pid} | ignore | {error,Error}
%% @end
%%--------------------------------------------------------------------
private_msg(User,Msg) ->
    case gen_server:call({global, chatterl_serv}, {user_lookup, User}, infinity) of
	{error, Error} -> {error, Error};
	{ok, _UserName, UserPid} ->
	    {name,From} = gen_server:call(UserPid, client_name, infinity),
	    CreatedOn = erlang:now(),
	    case gen_server:call(UserPid, {receive_msg, CreatedOn, From, Msg}, infinity) of
		ok ->
		    {ok, msg_sent};
		_ ->{error, "Unable to send message!"}
	    end
    end.
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @edoc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([User]) ->
    process_flag(trap_exit, true),
    case gen_server:call({global, chatterl_serv}, {connect, User}, infinity) of
	{error, Error} ->
	    io:format("~p~n", [Error]),
	    {stop, Error};
	{ok, Message} ->
	    io:format("~p is ~p.~n", [User, Message]),
	    {ok, #user{
	       name = User,
	      groups = gb_trees:empty()}}
    end.

%%--------------------------------------------------------------------
%% @edoc
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
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(client_name, _From, State) ->
    Reply = {name, State#user.name},
    {reply, Reply, State};
handle_call(groups, _From, State) ->
    Reply = gb_trees:values(State#user.groups),
    {reply, Reply, State};
handle_call({add_group, Group, Pid}, _From, State) ->
    NewTree = gb_trees:insert(Group, {Group, Pid}, State#user.groups),
    {reply, {ok, "Joined group"}, State#user{groups = NewTree}};
handle_call({drop_group, Group}, _From, State) ->
    NewTree = gb_trees:delete(Group, State#user.groups),
    {reply, ok, State#user{groups = NewTree}};
handle_call({receive_msg, _CreatedOn, User, Msg}, _From, State) ->
    io:format("Received msg from ~p: ~p~n", [User,Msg]),
    {reply, ok, State}.
    

%%--------------------------------------------------------------------
%% @edoc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @edoc
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
%% @edoc
%% Disconnects the user from the client.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    %GroupsList = gb_trees:to_list(State#user.groups),
    %lists:foreach(fun chatterl_client:drop/1, GroupsList),
    gen_server:call({global, chatterl_serv}, {disconnect,State#user.name}, infinity),
    io:format("~p is disconnecting...~p", [State#user.name,Reason]),
    ok.
%%--------------------------------------------------------------------
%% @edoc
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
