%%%-------------------------------------------------------------------
%%% @author Yomi Colledge <yomi@boodah.net>
%%% @doc
%%% Basic client for chat system, used to interact with chatterl_serv
%%% and chatterl_groups
%%% @end
%%% @copyright 2008 by Yomi Colledge <yomi@boodah.net>
%%%-------------------------------------------------------------------
-module(chatterl_client).

-behaviour(gen_server).

%% API
-export([start/1,stop/0,join/1,drop/1,connected_to/0,list_groups/0]).
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
%% Starts the client
%%
%% @spec start(Client) -> {ok,Pid} | ignore | {error,Error} 
%% @end
%%--------------------------------------------------------------------
start(Client) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Client], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the client
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
%% Allows a client to join a group
%%
%% @spec join(Group) -> {ok,Msg} | {error,Error}
%% @end
%%--------------------------------------------------------------------
join(Group) ->
    set_client_to_group(join,Group).

%%--------------------------------------------------------------------
%% @doc
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
    case gen_server:call({global, chatterl_serv}, {client_lookup, Client}, infinity) of
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
    Reply = {name, State#client.name},
    {reply, Reply, State};
handle_call(groups, _From, State) ->
    Reply = gb_trees:values(State#client.groups),
    {reply, Reply, State};
handle_call({add_group, Group, Pid}, _From, State) ->
    NewTree = gb_trees:insert(Group, {Group, Pid}, State#client.groups),
    {reply, {ok, "Joined group"}, State#client{groups = NewTree}};
handle_call({drop_group, Group}, _From, State) ->
    NewTree = gb_trees:delete(Group, State#client.groups),
    {reply, ok, State#client{groups = NewTree}};
handle_call({receive_msg, _CreatedOn, Client, Msg}, _From, State) ->
    io:format("Received msg from ~p: ~p~n", [Client,Msg]),
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
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% Disconnects the client from the client.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    GroupsList = gb_trees:values(State#client.groups),
    gen_server:call({global, chatterl_serv}, {disconnect,State#client.name,GroupsList}, infinity),
    io:format("~p is disconnecting...~p~n", [State#client.name,Reason]),
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
%% Determines the action that needs to be taken out on a group
%%
%% @spec send_msg(Group,Msg) -> {ok,Message} | {error,Error}
%% @end
%%--------------------------------------------------------------------
set_client_to_group(Action,Group) ->
    case gen_server:call({global, chatterl_serv}, {get_group, Group}, infinity) of
	{GroupName, GroupPid} ->
	    Response =
		case Action of
		    join ->
			{{join,GroupName,GroupPid},add_group};
		    drop ->
			{{drop,Group},drop_group};
		    _ -> {error, {"Illegal action",Action}}
		end,
	    group_connection(Response,GroupPid);
	false ->
	    {error, "Group doesn't exist!"}
    end.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sets up the connection between the client and the group
%%
%% @spec send_msg(Group,Msg) -> {ok,msg_sent} | {error,Error}
%% @end
%%--------------------------------------------------------------------
group_connection({GroupCall,ClientCall},GroupPid) ->
    case gen_server:call(chatterl_client, client_name, infinity) of
	{name, Name} -> 
	    case gen_server:call(GroupPid, {GroupCall, Name}, infinity) of
		{ok, Msg} ->
		    gen_server:call(chatterl_client, ClientCall, infinity),
		    {ok, Msg};
		_ -> {error, "Unable to connect!"}
	    end;
	_ -> 
	    {error, "Unknown error!"}
    end.
