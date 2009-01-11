%%%-------------------------------------------------------------------
%%% @author Yomi Colledge <yomi@boodah.net>
%%% @doc
%%% Basic client for chat system, used to interact with Chatterl.
%%%
%%% Each time a client starts a process they are automatically connected
%%% to a Chatterl server, once this is done the user is able to join
%%% various groups on the server.
%%%
%%% There are no restrictions to the number of groups a user can connect
%%% to, so the user is free to join as many as they please.
%%%
%%% Clients can send messages to a specific group or to another client as
%%% well as being able to what groups they are connected to.
%%%
%%% In the future we will have a client supervisor that will handle these
%%% processes, but not before we have branched off and started to implement
%%% a replacement client frontend for the system (Erlang and web based).
%%% @end
%%% @copyright 2008 by Yomi Colledge <yomi@boodah.net>
%%%-------------------------------------------------------------------
-module(chatterl_client).

-behaviour(gen_server).

%% API
-export([start/1,stop/0,join/1,drop/1,connected_to/0,list_users/1,list_groups/0]).
-export([name/0,private_msg/2,send_msg/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(client, {name, ip, groups}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Connects the client to Chatterl
%%
%% @spec start(Client) -> {ok,Pid} | ignore | {error,Error} 
%% @end
%%--------------------------------------------------------------------
start(Client) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Client], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the client, disconnecting them from Chatterl all together.
%%
%% Tells all joined groups and the server to drop the client from the
%% system.
%%
%% @spec stop() -> stopped
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:call(?MODULE, stop, infinity).

%%--------------------------------------------------------------------
%% @doc
%% Gets the clients name
%%
%% @spec name() -> {name,Name} | {error,Error}
%% @end
%%--------------------------------------------------------------------
name() ->
    gen_server:call(chatterl_client, client_name, infinity).

%%--------------------------------------------------------------------
%% @doc
%% Retrieves a list of groups the client is connected to
%%
%% @spec connected_to() -> [Groups] | []
%% @end
%%--------------------------------------------------------------------
connected_to() ->
    gen_server:call(chatterl_client, groups, infinity).

%%--------------------------------------------------------------------
%% @doc
%% Retrieves a list of avaliable groups
%%
%% @spec list_groups() -> [Groups] | []
%% @end
%%--------------------------------------------------------------------
list_groups() ->
    gen_server:call({global, chatterl_serv}, list_groups, infinity).

%%--------------------------------------------------------------------
%% @doc
%% Retrieves a list of users connect to a groups
%%
%% @spec list_users(GroupName) -> [Users] | []
%% @end
%%--------------------------------------------------------------------
list_users(GroupName) ->
    case chatterl_serv:list_users(GroupName) of
	{error, Error} ->
	    {error, Error};
	Users ->
	    clean_user_list([],Users)
    end.
    
%%--------------------------------------------------------------------
%% @doc
%% Allows a client to join a specific group
%%
%% @spec join(Group) -> {ok,Msg} | {error,Error}
%% @end
%%--------------------------------------------------------------------
join(Group) ->
    determine_group_action(join,Group).

%%--------------------------------------------------------------------
%% @doc
%% Allows the uesr to drop from a group
%%
%% @spec drop(Group) -> {ok,Msg} | {error,Error}
%% @end
%%--------------------------------------------------------------------
drop(Group) ->
    determine_group_action(drop,Group).

%%--------------------------------------------------------------------
%% @doc
%% Sends a message to a specific group
%%
%% @spec send_msg(Group,Msg) -> {ok,msg_sent} | {error,Error}
%% @end
%%--------------------------------------------------------------------
send_msg(Group,Msg) ->
    case gen_server:call({global, chatterl_serv}, {get_group, Group}, infinity) of
	false ->
	    {error, "Unable to send message"};
	{GroupName, GroupPid} ->
	    {name,Client} = gen_server:call(chatterl_client, client_name, infinity),
	    case gen_server:call(GroupPid, {send_msg, Client, Msg}, infinity) of
		{ok, msg_sent} ->
		    io:format("Sent message to: ~p~n",[GroupName]);
		{error, Error} ->
		    {error, Error}
	    end
    end.

%%--------------------------------------------------------------------
%% @doc
%% Sends a private message to a connected client.
%%
%% @spec private_msg(Client,Msg) -> {ok,Pid} | ignore | {error,Error}
%% @end
%%--------------------------------------------------------------------
private_msg(Client,Msg) ->
    case gen_server:call({global, chatterl_serv}, {user_lookup, Client}, infinity) of
	{error, Error} -> {error, Error};
	{ok, _ClientName, ClientPid} ->
	    {name,From} = gen_server:call(ClientPid, client_name, infinity),
	    case gen_server:call(ClientPid, {receive_msg, erlang:now(), From, Msg}, infinity) of
		ok -> {ok, msg_sent};
		_ -> {error, "Unable to send message!"}
	    end
    end.
%%====================================================================
%% gen_server callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% Checks to see if the client can connect to Chatterl, if an error is
%% encountered we stop the process as nothing can be done without it.
%% Otherwise it stores the clients name and initialises a list ready to
%% store the groups.
%%
%% @spec init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Client]) ->
    process_flag(trap_exit, true),
    case gen_server:call({global, chatterl_serv}, {connect, Client}, infinity) of
	{error, Error} ->
	    io:format("~p~n", [Error]),
	    {stop, Error};
	{ok, Message} ->
	    io:format("~p is ~p.~n", [Client, Message]),
	    {ok, #client{
	       name = Client,
	      groups = gb_trees:empty()}}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Handles all our calls to the client process
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
handle_call({stop,Reason},_From,State) ->
    {stop,Reason,stopped,State};
handle_call({group_msg,Group,Msg},_From,State) ->
    Reply = 
	case gen_server:call({global, chatterl_serv}, {get_group, Group}, infinity) of
	    false ->
		{error, "Unable to send message"};
	    {GroupName, GroupPid} ->
		case gen_server:call(GroupPid, {send_msg, State#client.name, Msg}, infinity) of
		    {ok, msg_sent} ->
			io:format("Sent message to: ~p~n",[GroupName]),
			{ok,group_msg_sent};
		    {error, Error} ->
			{error, Error}
		end
	end,
    {reply,Reply,State};
handle_call(client_name, _From, State) ->
    Reply = {name, State#client.name},
    {reply, Reply, State};
handle_call(groups, _From, State) ->
    Reply = gb_trees:values(State#client.groups),
    {reply, Reply, State};
handle_call({add_group, Group, Pid}, _From, State) ->
    io:format("Joining group: ~p~n",[Group]),
    NewTree = gb_trees:insert(Group, {Group, Pid}, State#client.groups),
    {reply, {ok, "Joined group"}, State#client{groups = NewTree}};
handle_call({drop_group, Group}, _From, State) ->
    io:format("Disconnecting from: ~p~n",[Group]),
    NewTree = gb_trees:delete(Group, State#client.groups),
    {reply, {ok,"Dropped from group"}, State#client{groups = NewTree}};
handle_call({receive_msg, _CreatedOn, Client, Msg}, _From, State) ->
    io:format("Received msg~p: ~p~n", [Client,Msg]),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @doc
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
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    case Info of
	{'EXIT', Pid, Why} ->
	    handle_call({stop,Why}, Pid, State);
	Unknown ->
	    io:format("Caught unhandled message: ~w\n", [Unknown])
    end,
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% Disconnects the client from the client.
%%
%% Removes the client from Chatterl.
%%
%% @todo Refactor so that the reason is passed with the disconnect.
%% @spec terminate(Reason, State) -> ok
%% @end
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    GroupsList = gb_trees:values(State#client.groups),
    gen_server:call({global, chatterl_serv},
		    {disconnect,State#client.name,GroupsList}, infinity),
    io:format("~p is disconnecting...~p~n", [State#client.name,Reason]),
    ok.
%%--------------------------------------------------------------------
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(OldVsn, State, _Extra) ->
    io:format("Updating system from:~p~n",[OldVsn]),
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Cleans list so that we only have the actual values.
%% Used to remove our pid/pid reference from our results.
%%
%% @spec clean_user_list(CleanList,Users) -> {ok,Message}
%%                                               | {error,Error}
%% @end
%%--------------------------------------------------------------------
clean_user_list(CleanList,[User|Users]) ->
    {Value,{_Pid,_PidRef}} = User,
    CleanedList = [Value|CleanList],
    clean_user_list(CleanedList,Users);
clean_user_list(CleanList,[]) ->
    CleanList.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determines the action that needs to be taken out on a group
%%
%% Is used by the client API to simply the calls made to drop and join
%% as both pieces of functionality are simular, it made sense to refactor
%% them into the below code.
%%
%% @spec determine_group_action(Action,Group) -> {ok,Message}
%%                                               | {error,Error}
%% @end
%%--------------------------------------------------------------------
determine_group_action(Action,Group) ->
    case gen_server:call({global, chatterl_serv}, {get_group, Group}, infinity) of
	{GroupName, GroupPid} ->
	    {GroupCall,ClientCall} =
		case Action of
		    join ->
			{join,{add_group,GroupName,GroupPid}};
		    drop ->
			{drop,{drop_group,GroupName}};
		    _ -> {error, {"Illegal action",Action}}
		end,
	    group_action(GroupCall,ClientCall,GroupPid);
	false ->
	    {error, "Group doesn't exist!"}
    end.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Makes a call to our Chatterl client.
%%
%% Used by determine_group_action to carry out a actual call to our
%% client process.
%%
%% @see determine_group_action
%% @spec group_action(GroupCall,ClientCall,GroupPid) -> {ok,joined_group} | {error,Error}
%% @end
%%--------------------------------------------------------------------
group_action(GroupCall,ClientCall,GroupPid) ->
    case gen_server:call(chatterl_client, client_name, infinity) of
	{name, Client} ->
	    case gen_server:call(GroupPid, {GroupCall, Client}, infinity) of
		{ok, _Msg} ->
		    gen_server:call(chatterl_client, ClientCall, infinity),
		    {ok, joined_group};
		_ -> {error, "Unable to connect!"}
	    end;
	_ ->
	    {error, "Unknown error!"}
    end.
