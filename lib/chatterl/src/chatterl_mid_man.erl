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
-export([start/0,connect/1,disconnect/1,list_users/0,list_groups/0,private_msg/3]).
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

connect(Client) ->
    {Type,Reply} = 
	case gen_server:call({global, ?SERVER}, {connect, Client}) of
	    {ok,_Msg} ->
		{"success",Client++" now connected"};
	    {error,_Error} ->
		{"failure","Unable to connect"}
	end,
    build_carrier(Type,Reply).

disconnect(Client) ->
    case gen_server:call({global, ?SERVER}, {disconnect, Client}) of
	{ok,Msg} ->
	    {ok,Msg};
	{error,Error} ->
	    {error,Error}
    end.

private_msg(Sender, Client, Message) ->
    case gen_server:call({global,chatterl_serv},{user_exists,Sender}) of
	true ->
	    gen_server:call({global, Sender}, {private_msg, Client, Message});
	false ->
	    {error,"Client does'nt exist!"}
    end.

list_users() ->
    {Type,Result} = 
	case gen_server:call({global,chatterl_serv},list_users) of
	    [] -> {"success",build_carrier("clients","")};
	    Clients -> 
		ClientsList = [build_carrier("client",Client)||Client <- Clients],
		{"success",build_carrier("clients",ClientsList)}
	end,
    build_carrier(Type,Result).

list_groups() ->
    gen_server:call({global, chatterl_serv}, list_groups, infinity).
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
handle_cast({private_msg, Sender, Client, Message}, State) ->
    case dict:find(Client, State) of
	error ->
	    io:format("user not connected");
	{ok, Pid} ->
	    Pid ! {private_msg, Sender, Client, Message}
    end,
    {noreply, State};
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
%% @spec build_carrier(Type,Message) -> Record
%%
%% @end
%%--------------------------------------------------------------------
build_carrier(Type,Message) ->
    #carrier{ type=Type, message=Message}.
