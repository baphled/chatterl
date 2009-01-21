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
%% Client based
-export([start/0,connect/1,disconnect/1,list_users/0,list_users/1]).
-export([group_join/2,group_leave/2,group_info/1,group_send/3,group_poll/1,group_list/0]).
-export([group_create/2,group_drop/1]).
-export([private_msg/3,poll_client/1]).
%% helpers
-export([build_carrier/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(carrier, {type,message}).
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
%% @spec connect(Client) -> {ResponseType,Message}
%% @end
%%--------------------------------------------------------------------
connect(Client) ->
    {Type,Reply} = 
		case gen_server:call({global, ?SERVER}, {connect, Client}) of
		    {ok,_Msg} ->
			{"success",Client++" now connected"};
		    {error,_Error} ->
			{"failure","Unable to connect"}
		end,
    build_carrier(Type,Reply).

%%--------------------------------------------------------------------
%% @doc Disconnects a client to the Chatterl system.
%%
%% @spec disconnect(Client) -> {ResponseType,Message}
%% @end
%%--------------------------------------------------------------------
disconnect(Client) ->
    {Type,Reply} = 
	case gen_server:call({global, ?SERVER}, {disconnect, Client}) of
	{ok,Msg} ->
	    {"success",Msg};
	{error,Error} ->
	    {"failure",Error}
    end,
    build_carrier(Type,Reply).

%%--------------------------------------------------------------------
%% @doc Lists the clients connected to Chatterl
%%
%% @spec list_users() -> {ResponseType,Message}
%% @end
%%--------------------------------------------------------------------
list_users() ->
    {Type,Result} = 
	case gen_server:call({global,chatterl_serv},list_users) of
	    [] -> {"success",build_carrier("clients","")};
	    Clients -> 
		ClientsList = [build_carrier("client",Client)||Client <- Clients],
		{"success",build_carrier("clients",ClientsList)}
	end,
    build_carrier(Type,Result).

list_users(Group) ->
    {Type,Record} = 
	case gen_server:call({global,chatterl_serv},{group_exists,Group}) of
	    true -> ClientsList = [build_carrier("client",Client)
				   || {Client,{_Pid,_Ref}} 
					  <- gen_server:call({global,Group},list_users)],
		    {"success",build_carrier("clients",ClientsList)};
	    false ->
		{"error","Group: "++ Group ++ " doesn't exist"}
	end,
    build_carrier(Type,Record).

group_list() ->
    {Type,Result} = 
	case gen_server:call({global, chatterl_serv}, list_groups, infinity) of
	    [] -> {"success",build_carrier("groups","")};
	    Groups -> 
		GroupsList = [build_carrier("group",Group)||Group <- Groups],
		{"success",build_carrier("groups",GroupsList)}
    end,
    build_carrier(Type,Result).

group_create(Group,Description) ->
    {Type,Result} = 
	case gen_server:call({global,chatterl_serv},{create,Group,Description}) of
	    {error,_Error} ->
		{"failure","Unable to create group"};
	    {ok,_GroupPid} ->
		{"success","Group added"}
	end,
    build_carrier(Type,Result).

group_drop(Group) ->
    {Type,Result} = 
	case gen_server:call({global,chatterl_serv},{drop,Group}) of
	    {error,{Error,_GroupName}} ->
		{"failure",Error};
	    {ok,ResponseMsg} ->
		{"success",ResponseMsg}
	end,
    build_carrier(Type,Result).

group_info(Group) ->
    {Type,Result} = 
	case gen_server:call({global,chatterl_serv},{group_exists,Group}) of
	    true ->
		
		Data = gen_server:call({global,chatterl_serv},{group_info,Group}),
		Results = [build_carrier(atom_to_list(Atom),Info)|| {Atom,Info} <- Data],
		{"success",
		 build_carrier("groups",Results)};
	    false ->
		{"error","Group doesn't exist!"}
	end,
    build_carrier(Type,Result).

group_join(Group,Client) ->
    {Type,Reply} = 
	case gen_server:call({global,chatterl_serv},{group_exists,Group}) of
	    true ->
		 case gen_server:call({global,Group},{join,Client}) of
			{ok,_} ->
			    {"success",Client ++ " joined group"};
			{error,Error} ->
			    {"failure",Error}
		    end;
	    false ->
		{"error","Group: "++ Group ++ " doesn't exist"}
	end,
    build_carrier(Type,Reply).

group_leave(Group,Client) ->
    {Type,Record} = 
	case gen_server:call({global,chatterl_serv},{user_exists,Client}) of
	    true ->
		case gen_server:call({global,Group},{leave,Client}) of
		    {ok, _ } ->
			{"success",Client ++ " has disconnected from " ++ Group};
		    {error,Error} ->
			{"failure",Error}
		end;
	    false ->
		{"failure","User not joined"}
	end,
    build_carrier(Type,Record).

group_send(Group,Sender,Message) ->
    {Type,Reply} =
	case gen_server:call({global,chatterl_serv},{group_exists,Group}) of
	    true ->
		case gen_server:call({global,Group},{send_msg,Sender,Message}, infinity) of
		    {ok,Msg} ->
			{"success",atom_to_list(Msg)};
		    {error,Error} ->
			{"failure",Error}
		end;
	    false ->
		{"failure","User not joined"}
	end,
    build_carrier(Type,Reply).

group_poll(Group) ->
     {Type,Result} = 
	case gen_server:call({global,chatterl_serv},{group_exists,Group}) of
	    true ->
		case gen_server:call({global,Group},poll_messages) of
		    [] -> {"success",build_carrier("messages","")};
		    Messages ->
			MessagesList = [build_carrier("message",format_messages(Message))||Message <- Messages],
			{"success",build_carrier("messages",MessagesList)}
		end;
	    false ->
		{"error","Group: "++ Group ++ " doesn't exist"}
	end,
    build_carrier(Type,Result).
%%--------------------------------------------------------------------
%% @doc Allows a client to send a private message to another client.
%%
%% @spec private_msg(Sender,Client,Message) -> {ResponseType,Message}
%% @end
%%--------------------------------------------------------------------
private_msg(Sender, Client, Message) ->
    {Type,Reply} = 
	case gen_server:call({global,chatterl_serv},{user_exists,Sender}) of
	    true ->
		case gen_server:call({global, Sender}, {private_msg, Client, Message}) of
		    {ok,_Msg} ->
			{"success","Sending msg..."};
		    {error,Error} ->
			{"failure",Error}
		end;
	    false ->
		{"failure","Client doesn't exist!"}
    end,
    build_carrier(Type,Reply).

%%--------------------------------------------------------------------
%% @doc Allows a client to retrieve private messages.
%%
%% @spec poll_client(Client) -> {ResponseType,Message}
%% @end
%%--------------------------------------------------------------------
poll_client(Client) ->
    {Type,Result} = 
	case gen_server:call({global,chatterl_serv},{user_exists,Client}) of
	    true ->
		case gen_server:call({global,Client},poll_messages) of
		    [] -> {"success",build_carrier("messages","")};
		    Messages ->
			MessagesList = [build_carrier("message",format_messages(Message))||Message <- Messages],
			{"success",build_carrier("messages",MessagesList)}
		end;
	    false ->
		{"error","Client: "++ Client ++ " doesn't exist"}
	end,
    build_carrier(Type,Result).

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
    io:format("Starting Chatterl Middle Man~n"),
    {ok, dict:new()}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
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
		{{error, duplicate_nick_found}, State}
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
    io:format("Terminating Chatterl Middle Man..."),
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
proxy_client(Messages) ->
    receive
	{private_msg, Sender, Client, Message} ->
	    io:format("Sending private message: ~s~n",[Message]),
	    gen_server:call({global,Client},{private_msg,Sender,Message}),
	    proxy_client(Messages);
	{stop,Client} ->
	    io:format("Proxy stopping...~s~n",[Client]),
	    gen_server:call({global,chatterl_serv},{disconnect,Client});
	Other -> io:format("unknown proxy request ~s~n",[Other]),
		 proxy_client(Messages)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Builds and returns our carrier message.
%%
%% This is used to pass around message retrieved from Chatterl.
%% @spec build_carrier(Type,Message) -> Carrier
%%
%% @end
%%--------------------------------------------------------------------
build_carrier(Type,Message) ->
    #carrier{ type=Type, message=Message}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Generates the record for joining a Chatterl group.
%%
%% Have a feeling this can be cleaned up or used in other places, so
%% I have place it here.
%% @spec format_messages(MessageCarrier) -> [MessageRecord]
%%
%% @end
%%--------------------------------------------------------------------
format_messages({Client,Date,Message}) ->
    [build_carrier("client",Client),build_carrier("date",Date),build_carrier("msgbody",Message)].
