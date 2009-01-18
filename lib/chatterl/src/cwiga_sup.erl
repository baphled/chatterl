%%%----------------------------------------------------------------
%%% @author  Yomi Akindayini <yomi@boodah.net>
%%% @doc Supervisor for the Chatterl server, is linked to the
%%% main Chatterl supervisor.
%%%
%%% Handles our Chatterl server, which is in charge of passing mesages
%%% around and managing groups.
%%% @end
%%% @copyright 2008 Yomi Akindayini
%%%----------------------------------------------------------------
-module(cwiga_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link(Port) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Port) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([Port]) ->
    process_flag(trap_exit, true),
    RestartStrategy = one_for_all,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    Gateway = {chatterl_gateway, {chatterl_gateway, start, [Port]},
              Restart, Shutdown, Type, [chatterl_serv]},

    %% Ideally we want this supervisor to be optional
    MidMan = {chatterl_mid_man, {chatterl_mid_man, start, []},
              Restart, Shutdown, Type, [chatterl_mid_man]},
    {ok, {SupFlags, [Gateway,MidMan]}}.
%%%===================================================================
%%% Internal functions
%%%===================================================================


