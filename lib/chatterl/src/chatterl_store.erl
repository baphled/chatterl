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
-export([start_link/1,stop/0,group/1,user/1,get_group/1,get_user/1]).
-export([registered/0,register/2,is_auth/2,auth/2]).

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
%% @spec start() -> {ok,Pid} | ignore | {error,Error}
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

register(Nickname,{User,Email,Password1,Password2}) ->
  case gen_server:call({global,chatterl_serv},{user_exists,Nickname}) of
    false ->
      {error,lists:append(Nickname," is not connected")};
    true ->
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
      end
  end.

is_auth(Username,Password) ->
  case auth(Username,Password) of
    {ok, _} -> true;
    {error,_} -> false
  end.

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

registered() ->
  Q = qlc:q([X || X <- mnesia:table(registered_user)]),
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
%% @spec group(Group) -> GroupState
%% @end
%%--------------------------------------------------------------------
get_group(GroupName) ->
  F = fun() -> qlc:e(qlc:q([X || X <- mnesia:table(group), X#group.name =:= GroupName])) end,
  {atomic,Result} = mnesia:transaction(F),
  Result.

%%--------------------------------------------------------------------
%% @doc
%% Retrieves a groups state.
%%
%% @spec group(Group) -> GroupState
%% @end
%%--------------------------------------------------------------------
get_user(ClientName) ->
  F = fun() -> qlc:e(qlc:q([X || X <- mnesia:table(client), X#client.name =:= ClientName])) end,
  {atomic,Result} = mnesia:transaction(F),
  Result.

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
init([Copies]) ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  try
    mnesia:table_info(type, group)
    catch
      exit: _ ->
        mnesia:create_table(group,
                    [{attributes, record_info(fields, group)},
                     {Copies, [node()]},
                     {type, bag}])
    end,
  try
    mnesia:table_info(type, client)
    catch
      exit: _ ->
        mnesia:create_table(client,
                    [{attributes, record_info(fields, client)},
                     {Copies, [node()]},
                     {type, bag}])
    end,
  try
    mnesia:table_info(type, registered_user)
    catch
      exit: _ ->
        mnesia:create_table(registered_user,
                    [{attributes, record_info(fields, registered_user)},
                     {Copies, [node()]},
                     {type, bag}])
    end,
  {ok,#state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
  {stop, normal, stopped, State}.

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

create_user(Nickname,{User,Email,Password}) ->
  Row = #registered_user{nick=Nickname,firstname=User,email=Email,password=erlang:md5(Password)},
  F = fun() -> mnesia:write(Row) end,
  case mnesia:transaction(F) of
    {aborted,Result} ->
      {aborted,Result};
    _ -> {ok,lists:append(Nickname," is registered")}
  end.
