%%%----------------------------------------------------------------
%%% @author  Yomi Colledge <yomi@boodah.net>
%%% @doc Chatterl Store
%%%
%%% Storage mechanism for Chatterl
%%% @end
%%% @copyright 2009 Yomi Colledge
%%%---------------------------------------------------------------
-module(chatterl_store).

-behaviour(gen_server).

%% API
-export([start_link/1,stop/0,group/1,user/1,get_group/1,get_user/1,get_registered/1,get_messages/1,get_logged_in/0,logged_in/1]).
-export([login/2,logout/1,registered/0,register/2,is_auth/2,auth/2,archive_msg/2,edit_profile/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("stdlib/include/qlc.hrl").
-include("chatterl.hrl").

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Copies) -> {ok,Pid} | ignore | {error,Error}
%% @end
%%--------------------------------------------------------------------
start_link(Copies) ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [Copies], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the server
%%
%% @spec stop() -> stopped
%% @end
%%--------------------------------------------------------------------
stop() ->
  gen_server:call({global,?MODULE}, stop, infinity).

%%--------------------------------------------------------------------
%% @doc
%% Allows a client to login to chatterl
%%
%% @spec login(Nickname,Password) -> {ok,Msg} | {error,ErrorMsg}
%% @end
%%--------------------------------------------------------------------
login(Nickname,Password) ->
  case get_registered(Nickname) of
    [] ->
      {error,"Not Registered"};
    [_Result] ->
      case is_auth(Nickname,Password) of
        false ->
          {error,"Unable to login"};
        true ->
          set_status(Nickname,login),
          {ok,"Logged in"}
      end
  end.

%%--------------------------------------------------------------------
%% @doc
%% Allows a client to logout to chatterl
%%
%% @spec logout(Nickname) -> {ok,Msg} | {error,ErrorMsg}
%% @end
%%--------------------------------------------------------------------
logout(Nickname) ->
  case logged_in(Nickname) of
    false ->
      {error,"Not logged in"};
    true ->
      set_status(Nickname,logout)
  end.

%%--------------------------------------------------------------------
%% @doc
%% Determines whether a client is logged in or not
%%
%% @spec logged_in(Nickname) -> true | false
%% @end
%%--------------------------------------------------------------------
logged_in(Nickname) ->
  Q = qlc:q([X#registered_user.nick || X <- mnesia:table(registered_user),
                                       X#registered_user.logged_in =:= 1,
                                       X#registered_user.nick =:= Nickname]),
  F = fun() -> qlc:e(Q) end,
  {atomic,Result} = mnesia:transaction(F),
  case Result of
    [] -> false;
    [Nickname] -> true
  end.

%%--------------------------------------------------------------------
%% @doc
%% Archives a registered clients message
%%
%% Archives a clients message when the client is not presently logged in
%%
%% @todo Refactor so that it is a handle method
%% @spec archive_msg(Sender,{CreatedOn,Recipient,Msg}) -> {ok,Msg} | {error,Error}
%% @end
%%--------------------------------------------------------------------
archive_msg(Sender,{CreatedOn,Recipient,Msg}) ->
  case logged_in(Sender) of
    false -> {error,lists:append(Sender," not logged in")};
    true ->
      case get_registered(Recipient) of
        [] -> {error,lists:append(Recipient," is not registered")};
        [_] ->
          Row = #messages{recipient=Recipient,created_on=CreatedOn,sender=Sender,msg=Msg},
          F = fun() -> mnesia:write(Row) end,
          case mnesia:transaction(F) of
            {aborted,Result} ->
              {aborted,Result};
            {atomic,ok} -> {ok,lists:append("Sent message to ",Recipient)}
          end
      end
  end.

%%--------------------------------------------------------------------
%% @doc
%% Registers a client to chatterl.
%%
%% @spec register(Nickname,{User,Email,Password1,Password2}) -> {ok,Msg} | {error,Msg}
%% @end
%%--------------------------------------------------------------------
register(Nickname,{User,Email,Password1,Password2}) ->
  case Password1 =:= Password2 of
    false ->
      {error,lists:append(Nickname,"'s passwords must match")};
    true ->
      case is_auth(Nickname,Password1) of
        true ->
          {error,lists:append(Nickname," is already registered")};
        false ->
          create_user(Nickname,{User,Email,Password1})
      end
  end.

%%--------------------------------------------------------------------
%% @doc
%% Determines whether a client is authorised or not
%%
%% @spec is_auth(Username,Password) -> true | false
%% @end
%%--------------------------------------------------------------------
is_auth(Username,Password) ->
  case auth(Username,Password) of
    {ok, _} -> true;
    {error,_} -> false
  end.

%%--------------------------------------------------------------------
%% @doc
%% Authorises a client.
%%
%% @spec auth(Username,Password) -> {ok,Msg} | {error,Msg}
%% @end
%%--------------------------------------------------------------------
auth(Username,Password) ->
  Q = qlc:q([X#registered_user.nick || X <- mnesia:table(registered_user),
                                       X#registered_user.nick =:= Username,
                                       X#registered_user.password =:= erlang:md5(Password)]),
  F = fun() -> qlc:e(Q) end,
  {atomic,Result} = mnesia:transaction(F),
  case Result of
    [Nick] ->
      {ok,lists:append(Nick," Authorized")};
    [] ->
      {error, lists:append("Unable to authorise ",Username)}
  end.

%%--------------------------------------------------------------------
%% @doc
%% Retrieves a list of registered clients.
%%
%% @spec registered() -> [RegisteredUsers]
%% @end
%%--------------------------------------------------------------------
registered() ->
  Q = qlc:q([{X#registered_user.nick,X#registered_user.firstname,X#registered_user.email} || X <- mnesia:table(registered_user)]),
  F = fun() -> qlc:e(Q) end,
  {atomic,Result} = mnesia:transaction(F),
  Result.

%%--------------------------------------------------------------------
%% @doc
%% Stores a groups state
%%
%% @spec group(Group) -> ok | {error,Error}
%% @end
%%--------------------------------------------------------------------
group(Group) ->
  case gen_server:call({global,chatterl_serv},{group_exists,Group}) of
    true ->
      State = gen_server:call({global,Group},get_state),
      F = fun() -> mnesia:write(State) end,
      {atomic,Result} = mnesia:transaction(F),
      Result;
    false -> {error,"Group doesn't exist"}
  end.

%%--------------------------------------------------------------------
%% @doc
%% Stores a groups state
%%
%% @spec user(User) -> ok | {error,Error}
%% @end
%%--------------------------------------------------------------------
user(User) ->
  case gen_server:call({global,chatterl_serv},{user_exists,User}) of
    true ->
      State = gen_server:call({global,User},get_state),
      F = fun() -> mnesia:write(State) end,
      {atomic,Result} = mnesia:transaction(F),
      Result;
    false -> {error,"User doesn't exist"}
  end.

%%--------------------------------------------------------------------
%% @doc
%% Retrieves a groups state.
%%
%% @spec get_group(GroupName) -> GroupState
%% @end
%%--------------------------------------------------------------------
get_group(GroupName) ->
 get(group,GroupName).

%%--------------------------------------------------------------------
%% @doc
%% Retrieves a groups state.
%%
%% @spec get_user(ClientName) -> ClientState
%% @end
%%--------------------------------------------------------------------
get_user(ClientName) ->
  get(client,ClientName).

%%--------------------------------------------------------------------
%% @doc
%% Retrieves a registered users.
%%
%% @spec get_registered(User) -> RegisteredUser
%% @end
%%--------------------------------------------------------------------
get_registered(User) ->
  get(registered_user,User).

%%--------------------------------------------------------------------
%% @doc
%% Retrieves a registered users messages.
%%
%% @spec get_messages(User) -> Messages
%% @end
%%--------------------------------------------------------------------
get_messages(User) ->
  F = fun() -> mnesia:read({messages,User}) end,
  {atomic,Result} = mnesia:transaction(F),
  Result.

%%--------------------------------------------------------------------
%% @doc
%% Retrieves a logged in users.
%%
%% @spec get_logged_in() -> RegisteredUser
%% @end
%%--------------------------------------------------------------------
get_logged_in() ->
  Q = qlc:q([X#registered_user.nick || X <- mnesia:table(registered_user),X#registered_user.logged_in =:= 1]),
  F = fun() -> qlc:e(Q) end,
  {atomic,Result} = mnesia:transaction(F),
  Result.

%%--------------------------------------------------------------------
%% @doc
%% Edit a registerd client profile
%%
%% @spec edit_profile(Nickname,{Key,Value}) -> {ok,Msg} | {error,Error}
%% @end
%%--------------------------------------------------------------------
edit_profile(Nickname,{Key,Value}) ->
  case logged_in(Nickname) of
    false -> {error,lists:append(Nickname," not logged in")};
    true ->
      case edit_profile_query(Nickname,{Key,Value}) of
        {aborted,_Error} -> {error,"Unable to update profile"};
        {atomic,_Result} -> {ok,"Updated profile"}
      end
  end.

%%====================================================================
%% gen_server callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Initiates the server
%%
%% @spec init([]) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Copies]) ->
  create_tables(Copies),
  {ok,#state{}}.

%%--------------------------------------------------------------------
%% @doc
%% Handling the call messages the storage process.
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
  {stop, normal, stopped, State}.

%%--------------------------------------------------------------------
%% @doc
%% Handling cast message
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
%% Terminates chatterl_serv
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
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
%% Create our tables
%%
%% @spec create_tables(Copies) -> {atomic,ok} | {abort,ErrorMsg}
%% @end
%%--------------------------------------------------------------------
create_tables(Copies) ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  try
    mnesia:table_info(type, group)
  catch
    exit: _ ->
      mnesia:create_table(group,
                          [{attributes, record_info(fields, group)},
                           {Copies, [node()]},
                           {type, ordered_set}])
  end,
  try
    mnesia:table_info(type, client)
  catch
    exit: _ ->
      mnesia:create_table(client,
                          [{attributes, record_info(fields, client)},
                           {Copies, [node()]},
                           {type, ordered_set}])
  end,
  try
    mnesia:table_info(type, registered_user)
  catch
    exit: _ ->
      mnesia:create_table(registered_user,
                          [{attributes, record_info(fields, registered_user)},
                           {Copies, [node()]},
                           {type, ordered_set}])
  end,
  try
    mnesia:table_info(type, messages)
  catch
    exit: _ ->
      mnesia:create_table(messages,
                          [{attributes, record_info(fields, messages)},
                           {Copies, [node()]},
                           {type, bag}])
  end.

%%--------------------------------------------------------------------
%% @doc
%% Creates a user with our registered_user table
%%
%% @spec create_user(Nickname,{User,Email,Password}) -> {abort,Result} | {ok,Msg}
%% @end
%%--------------------------------------------------------------------
create_user(Nickname,{User,Email,Password}) ->
  Row = #registered_user{nick=Nickname,firstname=User,email=Email,password=erlang:md5(Password),logged_in=0},
  F = fun() -> mnesia:write(Row) end,
  case mnesia:transaction(F) of
    {aborted,Result} ->
      {aborted,Result};
    _ -> {ok,lists:append(Nickname," is registered")}
  end.

%%--------------------------------------------------------------------
%% @doc
%% Retrieves a single record from a table
%%
%% @spec get(Table,Value) -> [{TableRecord}] | []
%% @end
%%--------------------------------------------------------------------
get(Table,Value) ->
  F = fun() -> mnesia:read({Table,Value}) end,
  {atomic,Result} = mnesia:transaction(F),
  Result.

%%--------------------------------------------------------------------
%% @doc
%% Sets a clients login status.
%%
%% @spec set_status(Nickname,Status) -> {ok,Msg} | {error,ErrorMsg}
%% @end
%%--------------------------------------------------------------------
set_status(Nickname,Status) ->
  Fun = fun() ->
            [U] = mnesia:read(registered_user,Nickname,write),
            New = U#registered_user{logged_in=determine_status(Status)},
            mnesia:write(New) end,
  case mnesia:transaction(Fun) of
    {atomic,ok} -> {ok,"Logged out"};
    {_,Error} -> {error,Error}
  end.

%%--------------------------------------------------------------------
%% @doc
%% Determines the login status we want to set
%%
%% @spec determine_status(Status) -> 0 | 1
%% @end
%%--------------------------------------------------------------------
determine_status(Status) ->
  case Status of
    logout -> 0;
    login -> 1
  end.

%%--------------------------------------------------------------------
%% @doc
%% Query to edit a clients profile
%%
%% @spec edit_profile_query(Nickname,{Key,Value}) -> MnesiaResult
%% @end
%%--------------------------------------------------------------------
edit_profile_query(Nickname,{Key,Value}) ->
  Fun =
    fun() ->
        [U] = mnesia:read(registered_user,Nickname,write),
        New =
          case Key of
            firstname -> U#registered_user{firstname=Value};
            email -> U#registered_user{email=Value};
            password -> U#registered_user{password=erlang:md5(Value)}
          end,
        mnesia:write(New)
    end,
  mnesia:transaction(Fun).
