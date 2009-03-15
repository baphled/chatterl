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
-export([start_link/1,stop/0,group/1,get_group/1]).

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
%% Retrieves a groups state.
%%
%% @spec group(Group) -> GroupState
%% @end
%%--------------------------------------------------------------------
get_group(GroupName) ->
  F = fun() -> qlc:e(qlc:q([X || X <- mnesia:table(group), X#group.name =:= GroupName])) end,
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
    mnesia:table_info(type, groups)
    catch
      exit: _ ->
        mnesia:create_table(group,
                    [{attributes, record_info(fields, group)},
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
