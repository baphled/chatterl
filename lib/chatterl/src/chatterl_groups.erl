%%%-------------------------------------------------------------------
%%% @author Yomi Colledge <yomi@boodah.net>
%%% @doc
%%% Chatterl groups process used to handle a specific groups data (clients,
%%% messages, etc).
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
%% Starts the group process.
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
    Group = gen_server:call({global,?MODULE},name,infinity),
    Users = gen_server:call({global,?MODULE},list_users,infinity),
    lists:foreach(fun(User) ->
			  gen_server:call({global,chatterl_client},{drop_group,Group},infinity) end, Users),
    gen_server:call({global,?MODULE},stop,infinity).
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initialises our group process.
%%
%% @spec init(Name,Description) -> {ok, State} |
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
		{{error, "Already joined"}, State};
	    false ->
		io:format("~p joined ~p~n", [User,State#group.name]),
		{{ok, "User added"}, gb_trees:insert(User, {User,From}, State#group.users)}
	end,
    {reply, Reply, State#group{users=NewTree}};
handle_call({drop, User}, _From, State) ->
    {Reply, NewTree} =
	case gb_trees:is_defined(User, State#group.users) of
	    true ->
		io:format("~p disconnected group:~p~n", [User,State#group.name]),
		{{ok, dropped},
		 gb_trees:delete(User, State#group.users)};
	    false ->
		{{error, "Not connected"}, State}
	end,
    {reply, Reply, State#group{users=NewTree}};
handle_call({send_msg, User, Message}, _From, State) ->
    {Reply, NewTree} =
	case gb_trees:is_defined(Message, State#group.messages) of
	    false ->
		io:format("~p: ~p~n", [User, Message]),
		{{ok, msg_sent},
		gb_trees:insert(Message, {User, Message}, State#group.messages)};
	    true ->
		{{error, already_sent}, State}
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
handle_info(_Info, State) ->
    {unknown, State}.

%%--------------------------------------------------------------------
%% @doc
%% Terminates the group process, making sure that all the users aswell
%% as the server know.
%%
%% @spec terminate(Reason, State) -> {shutdown,GroupName}
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    io:format("Processing shutting down ~p~n", [State#group.name]),
    case gb_trees:is_empty(State#group.users) of
	false ->
	    UsersList = gb_trees:values(State#group.users),
	    io:format("Send disconnects messages to ~p~n", [UserList]);
	true ->
	    io:format("No users to inform of shutdown~n")
    end,
    gen_server:call({global, chatterl_serv}, {remove_pid, State#group.name}, infinity),
    io:format("Shutdown ~p~n",[State#group.name]),
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
