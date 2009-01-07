%%%----------------------------------------------------------------
%%% @Author  Yomi Colledge <yomi@boodah.net>
%%% @doc Web interface for Chatterl
%%%
%%% Chatterl Administration web interface
%%%
%%% Chatterl Web Interface middleman.
%%%
%%% Sends messages to Chatterl Serv and manages clients connected
%%% to the interface.
%%% @end
%%% @copyright 2008 Yomi Colledge
%%%---------------------------------------------------------------
-behaviour(gen_server).

%% API
-export([start_link/0/connect/1,send_message/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

connect(Nickname) ->
  case gen_server:call({global, ?SERVER}, {register, Nickname}) of
    ok ->
      ok;
    {error, Error} ->
      Error
  end.

send_message(Sender, Addressee, Message) ->
  gen_server:cast({global, ?SERVER}, {send_message, Sender, Addressee, Message}).
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
    {ok, #state{}}.

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
  case dict:find(Nickname, State) of
    error ->
      Pid = spawn(fun() ->
		      process_flag(trap_exit, true),
		      proxy_client([]) end),
      erlang:monitor(process, Pid),
      gen_server:call({global, ?MODULE}, {connect,User}, infinity),
      {reply, ok, dict:store(Nickname, Pid, State)};
    {ok, _} ->
      {reply, {error, duplicate_nick_found}, State}
  end;
handle_cast({send_message, Sender, Addressee, Message}, State) ->
  case dict:find(Sender, State) of
    error ->
      ok;
    {ok, Pid} ->
      Pid ! {send_message, Addressee, Message}
  end,
  {noreply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
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
	{printmsg, MessageBody} ->
	    proxy_client([MessageBody|Messages]);
	{get_messages, Caller} ->
	    Caller ! {messages, lists:reverse(Messages)},
	    proxy_client([]);
	{send_message, Addressee, Message} ->
	    message_router:send_chat_message(Group,Message),
	    proxy_client(Messages);
	stop ->
	    io:format("Proxy stopping...~n"),
	    ok
    end.
