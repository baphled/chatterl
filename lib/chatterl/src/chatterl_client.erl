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
    gen_server:call({global,self()},{join_group,Group}).

%%--------------------------------------------------------------------
%% @doc
%% Allows the uesr to drop from a group
%%
%% @spec leave(Group) -> {ok,Msg} | {error,Error}
%% @end
%%--------------------------------------------------------------------
leave(Group) ->
    gen_server:call({global,self()},{leave_group,Group}).

%%--------------------------------------------------------------------
%% @doc
%% Allows the uesr to drop from a group
%%
%% @spec private_msg(Sender,Client,Message) -> {ok,Msg} | {error,Error}
%% @end
%%--------------------------------------------------------------------
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
	    {GroupName, _Description,GroupPid} ->
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
handle_call({join_group, Group}, _From, State) ->
  {NewTree,Result} =
    case gen_server:call({global, chatterl_serv}, {get_group, Group}, infinity) of
      {Group, _Description, GroupPid} ->
        case gen_server:call(GroupPid, {join, State#client.name}, infinity) of
          {ok, _Msg} ->
            {gb_trees:insert(Group, {Group, erlang:localtime(), GroupPid}, State#client.groups),
             {ok, joined_group}};
          _ -> {State#client.groups,{error, "Unable to connect!"}}
        end;
      _ ->
        {State#client.groups,{error, "Unknown error!"}}
    end,
  {reply, Result, State#client{groups = NewTree}};
handle_call({leave_group,Group},_From,State) ->
  {NewTree,Result} =
    case gen_server:call({global, chatterl_serv}, {get_group, Group}, infinity) of
      {Group, GroupPid} ->
        case gen_server:call(GroupPid, {leave, State#client.name}, infinity) of
          {ok, _Msg} ->
            {gb_trees:delete(Group, State#client.groups),
             {ok, drop_group}};
          _ -> {State#client.groups,{error, "Unable to disconnect!"}}
        end;
      _ ->
        {State#client.groups,{error, "Unknown error!"}}
    end,
  {reply, Result, State#client{groups = NewTree}};
handle_call({private_msg,Client,Message},_From,State) ->
    Result = case gen_server:call({global, chatterl_serv}, {user_lookup, Client}, infinity) of
	{error, Error} -> {error, Error};
	{ok, ClientName, _ClientPid} ->
		     case Client =:= {client,State#client.name} of
			 false ->
			     gen_server:call({global,Client},{receive_msg,erlang:localtime(),{client,ClientName},Message}),
			     {ok,msg_send};
			 true ->
			     {error, "Can not send to self!"}
		     end
	     end,
    {reply,Result,State};
handle_call({receive_msg, CreatedOn, Client, Msg}, _From, State) ->
    {Reply, NewTree} =
	case gb_trees:is_defined({Client,CreatedOn},State#client.messages) of
	    true ->
		{{error,no_duplicates},State#client.messages};
	    false ->
		{{ok,sent_msg},
		 gb_trees:insert({Client,CreatedOn}, {CreatedOn,Client,Msg}, State#client.messages)}
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
    io:format("~p is disconnected...~p~n", [State#client.name,Reason]),
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
