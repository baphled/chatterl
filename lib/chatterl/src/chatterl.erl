%%%----------------------------------------------------------------
%%% @author Yomi Colledge <yomi@boodah.net>
%%% @doc
%%% Initialises chatterl, a multiprocessed chat system, allowing one
%%% to create a number of chat groups to which clients can connect
%%% and send each other private messages.
%%%
%%% @end
%%% @copyright 2008 Yomi Colledge
%%%----------------------------------------------------------------,
-module(chatterl).

-behaviour(application).

%% Application callbacks
-export([start/0,start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% This function is used to call initialise all essential applications
%% linked to Chatterl and starts Chatterl itself.
%%
%% @spec start() -> {ok, Pid} | {ok, Pid, State} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
start() ->
    mochiweb:start(),
    application:start(?MODULE).
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, StartArgs) ->
    case chatterl_sup:start_link(StartArgs) of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================


