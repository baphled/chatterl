%%%----------------------------------------------------------------
%%% @author  Yomi Colledge <yomi@boodah.net>
%%% @doc Web interface for Chatterl
%%%
%%% Chatterl Web Interface middleman, all communication to Chatterl
%%% go via the middle man.
%%%
%%% Prepares &amp; sends messages to Chatterl Serv and manages
%%% clients connected to the interface.
%%% @end
%%% @copyright 2008-2009 Yomi Colledge
%%%---------------------------------------------------------------
-module(chatterl_mid_man).
-behaviour(gen_server).

%% API
%% Client based calls
-export([
         start/0,
         connect/2,
         disconnect/2,
         user_list/1,
         user_list/2,
         user_msg/2,
         user_poll/2,
         user_groups/2
        ]).

%% Group based calls
-export([
         group_join/2,
         group_leave/2,
         group_info/2,
         group_send/2,
         group_poll/2,
         group_list/1
        ]).

%% Admin based calls
-export([group_create/2,group_drop/2]).

%% gen_server callbacks
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
	 terminate/2,
         code_change/3
        ]).

%% Message handler methods used to handle our API calls
-import(message_handler, [
                          get_response_body/2,
                          build_carrier/2,
                          format_messages/1
                         ]).

-define(SERVER, ?MODULE).
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc Start Chatterl's middle man process.
%%
%% @spec start() -> {ok,Pid} | ignore | {error,Error}
%% @end
%%--------------------------------------------------------------------
start() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Connects a client to the Chatterl system.
%%
%% @spec connect(ContentType,Client) -> {ResponseType,Message}
%% @end
%%--------------------------------------------------------------------
connect(ContentType,Client) ->
  {Type,Reply} =
    case gen_server:call({global, ?SERVER}, {connect, Client},infinity) of
      {ok,_Msg} ->
        {"success",Client++" now connected"};
      {error,_Error} ->
        {"failure","Unable to connect"}
    end,
  get_response_body(ContentType,build_carrier(Type,Reply)).

%%--------------------------------------------------------------------
%% @doc Disconnects a client to the Chatterl system.
%%
%% @spec disconnect(ContentType,Client) -> {ResponseType,Message}
%% @end
%%--------------------------------------------------------------------
disconnect(ContentType,Client) ->
  {Type,Reply} =
    case gen_server:call({global, ?SERVER}, {disconnect, Client},infinity) of
      {ok,Msg} ->
        {"success",Msg};
      {error,Error} ->
        {"failure",Error}
    end,
  get_response_body(ContentType,build_carrier(Type,Reply)).

%%--------------------------------------------------------------------
%% @doc Lists the clients connected to Chatterl
%%
%% @spec user_list(ContentType) -> {carrier,ResponseType,Message}
%% @end
%%--------------------------------------------------------------------
user_list(ContentType) ->
  {Type,Result} =
    case gen_server:call({global,chatterl_serv},list_users,infinity) of
      [] -> {"success",build_carrier("clients","")};
      Clients ->
        ClientsList = [build_carrier("client",Client)||Client <- Clients],
        {"success",build_carrier("clients",ClientsList)}
    end,
  get_response_body(ContentType,build_carrier(Type,Result)).

%%--------------------------------------------------------------------
%% @doc List users connected to a specified group
%%
%% @spec user_list(ContentType,Group) -> {carrier,ResponseType,Message}
%% @end
%%--------------------------------------------------------------------
user_list(ContentType,Group) ->
  {Type,Record} =
    case gen_server:call({global,chatterl_serv},{group_exists,Group},infinity) of
      true -> ClientsList = [build_carrier("client",Client)
                             || {Client,{_Pid,_Ref}}
                                  <- gen_server:call({global,Group},list_users)],
              {"success",build_carrier("clients",ClientsList)};
      false ->
        {"error","Group: "++ Group ++ " doesn't exist"}
    end,
  get_response_body(ContentType,build_carrier(Type,Record)).

%%--------------------------------------------------------------------
%% @doc Allows a client to send a private message to another client.
%%
%% @spec user_msg(ContentType,{Sender,Client,Message}) -> {carrier,ResponseType,Message}
%% @end
%%--------------------------------------------------------------------
user_msg(ContentType,{Sender, Client, Message}) ->
  {Type,Reply} =
    case gen_server:call({global,chatterl_serv},{user_exists,Sender},infinity) of
      true ->
        case gen_server:call({global, ?MODULE}, {private_msg, Sender, Client, Message},infinity) of
          {ok,_Msg} ->
            {"success","Sending msg..."};
          {error,Error} ->
            {"failure",Error}
        end;
      false ->
        {"failure","Client doesn't exist!"}
    end,
  get_response_body(ContentType,build_carrier(Type,Reply)).

%%--------------------------------------------------------------------
%% @doc Allows a client to retrieve private messages.
%%
%% @spec user_poll(ContentType,Client) -> {carrier,ResponseType,Message}
%% @end
%%--------------------------------------------------------------------
user_poll(ContentType,Client) ->
  {Type,Result} =
    case gen_server:call({global,chatterl_serv},{user_exists,Client}) of
      true ->
        case gen_server:call({global,Client},poll_messages,infinity) of
          [] -> {"success",build_carrier("messages","")};
          Messages ->
            MessagesList = [build_carrier("message",format_messages(Message))  || Message <- Messages],
            {"success",build_carrier("messages",MessagesList)}
        end;
      false ->
        {"error","Client: "++ Client ++ " doesn't exist"}
    end,
  get_response_body(ContentType,build_carrier(Type,Result)).

%%--------------------------------------------------------------------
%% @doc Retrieves a list of groups a client is joined to
%%
%% @spec user_groups(ContentType,Client) -> {carrier,ResponseType,Message}
%% @end
%%--------------------------------------------------------------------
user_groups(ContentType,Client) ->
  {Type,Result} =
    case gen_server:call({global,chatterl_serv},{user_exists,Client},infinity) of
      true ->
        case gen_server:call({global,Client},groups) of
          [] ->
             {"success",build_carrier("groups","")};
          Groups ->
            GroupsList = [build_carrier("group",Group) || {Group,_JoinedOn,_Pid} <- Groups],
            {"success",build_carrier("groups",GroupsList)}
        end;
      false ->
        {"error","Client: " ++ Client ++ " doesn't exist"}
    end,
  get_response_body(ContentType,build_carrier(Type,Result)).

%%--------------------------------------------------------------------
%% @doc lists the groups on Chatterl
%%
%% @spec group_list(ContentType) -> {carrier,ResponseType,Message}
%% @end
%%--------------------------------------------------------------------
group_list(ContentType) ->
  {Type,Result} =
    case gen_server:call({global, chatterl_serv}, list_groups, infinity) of
      [] -> {"success",build_carrier("groups","")};
      Groups ->
        GroupsList = [build_carrier("group",Group)||Group <- Groups],
        {"success",build_carrier("groups",GroupsList)}
    end,
  get_response_body(ContentType,build_carrier(Type,Result)).

%%--------------------------------------------------------------------
%% @doc Creates a group on Chatterl.
%%
%% @spec group_create(ContentType,{Group,Description}) -> {carrier,ResponseType,Message}
%% @end
%%--------------------------------------------------------------------
group_create(ContentType,{Group,Description}) ->
  {Type,Result} =
    case gen_server:call({global,chatterl_serv},{create,Group,Description}, infinity) of
      {error,_Error} ->
        {"failure","Unable to create group"};
      {ok,_GroupPid} ->
        {"success","Group added"};
      _ -> {"error","Unknown response."}
    end,
  get_response_body(ContentType,build_carrier(Type,Result)).

%%--------------------------------------------------------------------
%% @doc Drops a group from Chatterl.
%%
%% @spec group_drop(ContentType,Group) -> {carrier,ResponseType,Message}
%% @end
%%--------------------------------------------------------------------
group_drop(ContentType,Group) ->
  {Type,Result} =
    case gen_server:call({global,chatterl_serv},{drop,Group},infinity) of
      {error,{Error,_GroupName}} ->
        {"failure",Error};
      {ok,ResponseMsg} ->
        {"success",ResponseMsg}
    end,
  get_response_body(ContentType,build_carrier(Type,Result)).

%%--------------------------------------------------------------------
%% @doc Retrieves information on a specified Chatterl group.
%%
%% @spec group_info(ContentType,Group) -> {carrier,ResponseType,Message}
%% @end
%%--------------------------------------------------------------------
group_info(ContentType,Group) ->
  {Type,Result} =
    case gen_server:call({global,chatterl_serv},{group_exists,Group},infinity) of
      true ->
        Data = gen_server:call({global,chatterl_serv},{group_info,Group},infinity),
        Results = [build_carrier(atom_to_list(Atom),Info)|| {Atom,Info} <- Data],
        {"success",build_carrier("groups",Results)};
      false ->
        {"error","Group doesn't exist!"}
    end,
  get_response_body(ContentType,build_carrier(Type,Result)).

%%--------------------------------------------------------------------
%% @doc Allows a client to join a Chatterl group.
%%
%% @spec group_join(ContentType,{Group,Client}) -> {carrier,ResponseType,Message}
%% @end
%%--------------------------------------------------------------------
group_join(ContentType,{Group,Client}) ->
  {Type,Reply} =
	case gen_server:call({global,chatterl_serv},{group_exists,Group},infinity) of
	    true ->
		 case gen_server:call({global,Client},{join_group,Group},infinity) of
			{ok,_} ->
			    {"success",Client ++ " joined group"};
			{error,Error} ->
			    {"failure",Error}
		    end;
	    false ->
		{"error","Group: "++ Group ++ " doesn't exist"}
	end,
  get_response_body(ContentType,build_carrier(Type,Reply)).

%%--------------------------------------------------------------------
%% @doc Allows a client to leave a Chatterl group.
%%
%% @spec group_leave(ContentType,{Group,Client}) -> {carrier,ResponseType,Message}
%% @end
%%--------------------------------------------------------------------
group_leave(ContentType,{Group,Client}) ->
    {Type,Record} =
	case gen_server:call({global,chatterl_serv},{user_exists,Client},infinity) of
	    true ->
		case gen_server:call({global,Client},{leave_group,Group},infinity) of
		    {ok, _ } ->
			{"success",Client ++ " has disconnected from " ++ Group};
		    {error,Error} ->
			{"failure",Error}
		end;
	    false ->
		{"failure","User not joined"}
	end,
  get_response_body(ContentType,build_carrier(Type,Record)).

%%--------------------------------------------------------------------
%% @doc Allows a client to send a message to a Chatterl group.
%%
%% @spec group_send(ContentType,{Group,Sender,Message}) -> {carrier,ResponseType,Message}
%% @end
%%--------------------------------------------------------------------
group_send(ContentType,{Group,Sender,Message}) ->
    {Type,Reply} =
	case gen_server:call({global,chatterl_serv},{group_exists,Group},infinity) of
	    true ->
		case gen_server:call({global,?MODULE},{group_msg,Sender,Group,Message}, infinity) of
		    {ok,Msg} ->
			{"success",Msg};
		    {error,Error} ->
			{"failure",Error}
		end;
	    false ->
		{"failure","User not joined"}
	end,
  get_response_body(ContentType,build_carrier(Type,Reply)).

%%--------------------------------------------------------------------
%% @doc Allows a client to retrieve messages from a Chatterl group.
%%
%% @spec group_poll(ContentType,Group) -> {carrier,ResponseType,Message}
%% @end
%%--------------------------------------------------------------------
group_poll(ContentType,Group) ->
     {Type,Result} =
	case gen_server:call({global,chatterl_serv},{group_exists,Group},infinity) of
	    true ->
		case gen_server:call({global,Group},poll_messages,infinity) of
		    [] -> {"success",build_carrier("messages","")};
		    Messages ->
			MessagesList = [build_carrier("message",format_messages(Message))||Message <- Messages],
			{"success",build_carrier("messages",MessagesList)}
		end;
	    false ->
		{"error","Group: "++ Group ++ " doesn't exist"}
	end,
  get_response_body(ContentType,build_carrier(Type,Result)).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initialises Chatterl middle man process.
%%
%% @spec init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    io:format("Starting Chatterl Middle Man~n"),
    {ok, dict:new()}.

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
handle_call({connect, Nickname}, _From, State) ->
    {Reply,NewState} =
	case dict:find(Nickname, State) of
	    error ->
		Pid = spawn(fun() ->
				    process_flag(trap_exit, true),
				    proxy_client([]) end),
		erlang:monitor(process, Pid),
		{chatterl_client:start(Nickname), dict:store(Nickname, Pid, State)};
	    {ok, _} ->
		{{error, "Already connected"}, State}
    end,
    {reply,Reply,NewState};
handle_call({disconnect, Client}, _From,State) ->
    {Reply,NewState} =
	case dict:find(Client, State) of
	    error ->
		{{error,"Not connected"},State};
	    {ok, Pid} ->
		Pid ! {stop,Client},
		{{ok,"Disconnected"},dict:erase(Client, State)}
	end,
    {reply, Reply, NewState};
handle_call({group_msg, Sender, Group, Message}, _From, State) ->
    Reply = case dict:find(Sender, State) of
		error ->
		    io:format("~s is not joined to group: ~s~n",[Sender,Group]),
		    {error,"Unable to send msg!"};
		{ok, Pid} ->
		    Pid ! {group_msg, Sender, Group, Message},
		    {ok,"Message sent"}
	    end,
    {reply, Reply, State};
handle_call({private_msg, Sender, Client, Message}, _From, State) ->
    Reply =
	case dict:find(Client, State) of
	    error ->
		io:format("user not connected"),
		{error,"Unable to connect!"};
	    {ok, Pid} ->
		Pid ! {private_msg, Sender, Client, Message},
		{ok,"Sending msg"}
	end,
    {reply, Reply, State}.

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
%% Terminates the process.
%%
%% @spec terminate(Reason, State) -> {shutdown,GroupName}
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    io:format("Terminating Chatterl Middle Man..."),
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
%% @doc
%% Send our client messages to Chatterl
%%
%% @spec proxy_client(Messages) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
proxy_client(Messages) ->
    receive
      {'Exit',_SomePid,Reason} ->
        io:format("Crash: ~s~n",[Reason]);
      {private_msg, Sender, Client, Message} ->
        gen_server:call({global,Sender},{private_msg,Client,Message},infinity),
        io:format("Sent private message~n"),
        proxy_client(Messages);
      {group_msg, Sender, Group, Message} ->
        gen_server:call({global,Group},{send_msg,Sender,Message},infinity),
        io:format("Sent message from ~s to group: ~s~n", [Sender,Group]),
        proxy_client(Messages);
      {stop,Client} ->
        chatterl_client:stop(Client),
        io:format("Stopped proxy for ~s~n",[Client]);
      Other ->
        io:format("unknown proxy request ~s~n",[Other]),
        proxy_client(Messages)
    end.
