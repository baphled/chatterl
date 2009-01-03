%%%-------------------------------------------------------------------
%%% @author Yomi Colledge <yomi@boodah.net>
%%% @doc
%%% Chatterl groups process code used to handle a specific groups data
%%% (clients, messages, etc).
%%%
%%% Can be started independantly of via
%%% <code>chatterl_groups:start(GroupName,Description).</code>
%%% or via the Chatterl server:
%%% <code>chatterl_serv:create(GroupName,Description).</code>
%%% Both function in exactly the same manner so it is entirely up to the
%%% group creator.
%%%
%%% the system has been setup so that groups can be started as long
%%% as they can locate the Chatterl server. This allows us to have a
%%% number of groups per node over a number of servers.
%%%
%%% As of this writting groups will be shutdown automatically if the
%%% server is terminated/stop, this will be modified once the storage
%%% system has been implemented.
%%% @end
%%% @copyright 2008 by Yomi Colledge <yomi@boodah.net>
%%%-------------------------------------------------------------------
-module(chatterl_groups).
-behaviour(gen_server).

%% API
-export([start/2,stop/0]).

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
%% Starts the group process, passing the group name and description.
%%
%% @spec start(Name,Description) -> {ok,Pid} | ignore | {error,Error} 
%% @end
%%--------------------------------------------------------------------
start(Name,Description) ->
    gen_server:start_link({global, Name}, ?MODULE, [Name,Description], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the process, sending messages to all clients connected and to the
%% server handling its process and information.
%%
%% @spec stop() -> stopped
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:call({global,?MODULE},stop,infinity).
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initialises our group process.
%%
%% @spec init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Name,Description]) ->
    process_flag(trap_exit, true),
    io:format("Initialising ~p...~n", [Name]),
    {ok,
     #group{
       name = Name,
       description = Description,
       messages = gb_trees:empty(),
       users = gb_trees:empty()}}.

%%--------------------------------------------------------------------
%% @doc
%% Handles our call messages
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
    io:format("Processing shut down ~p~n", [State#group.name]),
    {stop, normal, stopped, State};
handle_call(name, _From, State) ->
    Result = State#group.name,
    Reply = {name, Result},
    {reply, Reply, State};
handle_call(description, _From, State) ->
    Result = State#group.description,
    Reply = {description, Result},
    {reply, Reply, State};
handle_call(list_users, _From, State) ->
    Reply = gb_trees:values(State#group.users),
    {reply, Reply, State};
handle_call({join, User}, From, State) ->
    {Reply, NewTree} =
	case gb_trees:is_defined(User, State#group.users) of
	    true ->
		{{error, "Already joined"}, State#group.users};
	    false ->
		io:format("~p joined ~p~n", [User,State#group.name]),
		{{ok, "User added"}, gb_trees:insert(User, {User,From}, State#group.users)}
	end,
    {reply, Reply, State#group{users=NewTree}};
handle_call({drop, User}, _From, State) ->
    {Reply, NewTree} =
	case gb_trees:is_defined(User, State#group.users) of
	    true ->
		io:format("~p disconnected from group:~p~n",[User,State#group.name]),
		{{ok, dropped},gb_trees:delete(User, State#group.users)};
	    false ->
		{{error, "Not connected"}, State#group.users}
	end,
    {reply, Reply, State#group{users=NewTree}};
handle_call({send_msg,User,Message},_From,State) ->
    {Reply, NewTree} =
	case gb_trees:is_defined(Message, State#group.messages) of
	    false ->
		CreatedOn = erlang:now(),
		io:format("~p: ~p~n", [User,Message]),
		determine_user_action(State#group.name,{receive_msg,{CreatedOn,User,Message}},
				      gb_trees:values(State#group.users)),
		{{ok, msg_sent},
		 gb_trees:insert(Message, {User,CreatedOn,Message}, State#group.messages)};
	    true ->
		{{error, already_sent}, State#group.messages}
	end,
    {reply, Reply, State#group{messages=NewTree}}.
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
	Unknown ->
	    io:format("Caught unhandled message: ~w\n", [Unknown])
    end,
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% Terminates the group process, making sure that all the users
%% connected to the group as the server know of the termination.
%%
%% @spec terminate(Reason, State) -> {shutdown,GroupName}
%% @end
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    case gb_trees:is_empty(State#group.users) of
	false ->
	    gen_server:call({global,chatterl_serv}, {remove_pid,State#group.name},infinity),
	    determine_user_action(State#group.name,{drop_group,[]},
				  gb_trees:values(State#group.users));
	true ->
	    io:format("No users to inform of shutdown~n")
    end,
    io:format("Shutdown ~p:~nReason:~p~n",[State#group.name,Reason]),
    {shutdown, State#group.name}.

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
%% Determines the kind of action that needs to be taken out and calls
%% send_msg_to_users.
%%
%% @spec determine_user_action(GroupName,{Action,PayLoad},UsersList) ->
%%                                                       {error,Error}
%%                                                       | void()
%% @end
%%--------------------------------------------------------------------
determine_user_action(GroupName,{Action,PayLoad},UsersList) ->
    case Action of
	drop_group ->
	    GroupMsg = "Sending disconnect message to ~p~n",
	    send_msg_to_users({drop_group,GroupName},UsersList,GroupMsg);
	receive_msg ->
	    case PayLoad of
		{CreatedOn,Sender,Message} ->
		    GroupMsg = "Sending to users ~p~n",
		    send_msg_to_users({receive_msg, CreatedOn,Sender,Message},UsersList,GroupMsg);
		_ ->
		    {error, "Illegal payload format"}
	    end;
	_ -> {error, "Illegal action!"}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sends processs messages to all connected clients
%%
%% @spec send_msg_to_users(PayLoad,UsersList,GroupMsg) ->
%%                                                       {error,Error}
%%                                                       | void()
%% @end
%%--------------------------------------------------------------------
send_msg_to_users(PayLoad,UsersList,GroupMsg) ->
    lists:foreach(
      fun(User) ->
	      {Client,_PidInfo} = User,
	      case gen_server:call({global, chatterl_serv},
				   {user_lookup, Client}, infinity) of
		  {error, Error} ->
		      io:format("Error: ~p~n",[Error]);
		  {ok, ClientName, ClientPid} ->
		      io:format(GroupMsg,
				[ClientName]),
		      gen_server:call(ClientPid,PayLoad)
	      end
      end,
      UsersList).

