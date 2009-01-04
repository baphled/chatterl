%%%----------------------------------------------------------------
%%% @author  Yomi Akindayini <yomi@boodah.net>
%%% @doc
%%% Supervisor for the Chatterl Web Interfaces, is linked to the
%%% main Chatterl supervisor.
%%%
%%% Handles our Chatterl web interfaces taking the port as a parameter,
%%% primarily the admin cp. This supervisor will ultimately supervise
%%% both the admin & server web interface processes.
%%% @end
%%% @copyright 2008 Yomi Akindayini
%%%----------------------------------------------------------------
-module(web_sup).

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
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    WebInterface = {chatterl_web, {chatterl_web, start, [Port]},
              Restart, Shutdown, Type, [chatterl_web]},
    {ok, {SupFlags, [WebInterface]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


