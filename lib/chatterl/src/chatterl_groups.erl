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
%%% @copyright 2008-2009 by Yomi Colledge <yomi@boodah.net>
%%%-------------------------------------------------------------------
-module(chatterl_groups).
-behaviour(gen_server).

%% API
-export([start/2,stop/1]).

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
start(Group,Description) ->
    gen_server:start_link({global, Group}, ?MODULE, [Group,Description], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the process, sending messages to all clients connected and to the
%% server handling its process and information.
%%
%% @spec stop(Group) -> stopped
%% @end
%%--------------------------------------------------------------------
stop(Group) ->
    gen_server:call({global,Group},stop,infinity).

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
    %io:format("Initialising ~s...~n", [Name]),
    {ok,
     #group{
       name = Name,
       description = Description,
       created = erlang:localtime(),
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
    io:format("Processing shut down ~s~n", [State#group.name]),
    {stop, normal, stopped, State};
handle_call(name, _From, State) ->
    Result = State#group.name,
    Reply = {name, Result},
    {reply, Reply, State};
handle_call(description, _From, State) ->
    Result = State#group.description,
    Reply = {description, Result},
    {reply, Reply, State};
handle_call(created, _From, State) ->
    Result = State#group.created,
    Reply = {created, Result},
    {reply, Reply, State};
handle_call(poll_messages, _From, State) ->
    {reply, gb_trees:values(State#group.messages), State};
handle_call(list_users, _From, State) ->
    Reply = gb_trees:values(State#group.users),
    {reply, Reply, State};
handle_call({join, User}, From, State) ->
  {Reply, NewTree} =
    case gb_trees:is_defined(User, State#group.users) of
      true ->
        {{error, "Already joined"}, State#group.users};
      false ->
        case is_list(User) of
          true ->
            case gen_server:call({global,chatterl_serv},{user_exists,User}) of
              true ->
                io:format("~s joined ~s~n", [User,State#group.name]),
                {{ok, "User added"},
                 gb_trees:insert(User, {User,From}, State#group.users)};
              false ->
                {{error, "not connected"}, State#group.users}
            end;
          false ->io:format("~s not a valid user",[undefined]),
                  {{error, "Invalid user name"}, State#group.users}
        end
    end,
  {reply, Reply, State#group{users=NewTree}};
handle_call({leave, User}, _From, State) ->
    {Reply, NewTree} =
	case gb_trees:is_defined(User, State#group.users) of
	    true ->
		io:format("~s disconnected from group:~s~n",[User,State#group.name]),
		{{ok, dropped},gb_trees:delete(User, State#group.users)};
	    false ->
		{{error, "Not connected"}, State#group.users}
	end,
    {reply, Reply, State#group{users=NewTree}};
handle_call({send_msg,User,Message},_From,State) ->
    {Reply, NewTree} =
    case gb_trees:is_defined(User, State#group.users) of
      false ->
        {{error, user_not_joined}, State#group.messages};
      true ->
        CreatedOn = erlang:localtime(),
        case gb_trees:is_defined({User,CreatedOn}, State#group.messages) of
          false ->
            determine_user_action(State#group.name,{receive_msg,{CreatedOn,{group,State#group.name},User ++ ": " ++Message}},
                                  gb_trees:values(State#group.users)),
            {{ok, msg_sent},
             gb_trees:insert({User,CreatedOn}, {CreatedOn,{client,User},Message}, State#group.messages)};
          true ->
            {{error, already_sent}, State#group.messages}
        end
    end,
  {reply, Reply, State#group{messages=NewTree}};
handle_call({user_exists,User},_From,State) ->
  {reply,gb_trees:is_defined(User,State#group.users),State};
handle_call(get_state,_From,State) ->
  {reply,State,State}.
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
	    determine_user_action(State#group.name,{drop_group,[]},
				  gb_trees:values(State#group.users)),
	    io:format("Shutting down, need to tell clients...~n");
	true ->
	    io:format("No users to inform of shutdown~n")
    end,
    io:format("Shutdown ~s:~nReason:~s~n",[State#group.name,Reason]),
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
%% Determines the kind of action that needs to be taken and calls
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
	    GroupMsg = "Sending disconnect message to ~s~n",
	    send_msg_to_users({left_group,GroupName},UsersList,GroupMsg);
	receive_msg ->
	    case PayLoad of
		{CreatedOn,Sender,Message} ->
		    GroupMsg = "Sending to message to user: ~s~n",
		    send_msg_to_users({receive_msg,CreatedOn,Sender,Message},UsersList,GroupMsg);
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
          io:format(GroupMsg, [Client]),
          gen_server:call({global,Client},PayLoad)
      end,
      UsersList).
