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
%%% @end
%%% @copyright 2008-2009 by Yomi Colledge <yomi@boodah.net>
%%%-------------------------------------------------------------------
-module(chatterl_client).

-behaviour(gen_server).

%% API
%% Client based
-export([start/1,stop/1,private_msg/3]).
%% Group based
-export([join/1,leave/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(client, {name, ip, groups, messages}).

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
    gen_server:start_link({global, Client}, ?MODULE, [Client], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the client, disconnecting them from Chatterl all together.
%%
%% Tells all joined groups and the server to drop the client from the
%% system.
%%
%% @spec stop(Client) -> stopped
%% @end
%%--------------------------------------------------------------------
stop(Client) ->
    gen_server:call({global,Client}, stop, infinity).

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
%% @spec leave(Group) -> {ok,Msg} | {error,Error}
%% @end
%%--------------------------------------------------------------------
leave(Group) ->
    determine_group_action(leave,Group).

private_msg(Sender,Client,Message) ->
    gen_server:call({global,Sender},{private_msg,Client,Message}).
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
	       messages = gb_trees:empty(),
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
handle_call({private_msg,Client,Message},_From,State) ->
    Result = case gen_server:call({global, chatterl_serv}, {user_lookup, Client}, infinity) of
	{error, Error} -> {error, Error};
	{ok, ClientName, _ClientPid} ->
		     case ClientName =:= State#client.name of
			 false ->
			     gen_server:call({global,Client},{receive_msg, erlang:localtime(),State#client.name ,Message}),
			     {ok,msg_send};
			 true ->
			     {error, "Can not send to self!"}
		     end
	     end,
    {reply,Result,State};
handle_call({receive_msg, CreatedOn, Client, Msg}, _From, State) ->
    io:format("~p:~p - ~p~n", [httpd_util:rfc1123_date(CreatedOn),Client,Msg]),
    {Reply, NewTree} = 
	case gb_trees:is_defined({Client,CreatedOn},State#client.messages) of
	    true ->
		{{error,no_duplicates},State#client.messages};
	    false ->
		{{ok,sent_msg},
		 gb_trees:insert({Client,CreatedOn}, {Client,CreatedOn,Msg}, State#client.messages)}
	end,
    {reply, Reply, State#client{messages = NewTree}};
handle_call(poll_messages, _From,State) ->
    {reply,gb_trees:values(State#client.messages),State}.

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
		    leave ->
			{leave,{drop_group,GroupName}};
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
    case gen_server:call(self(), client_name, infinity) of
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
