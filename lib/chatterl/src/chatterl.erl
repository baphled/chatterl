%%%-------------------------------------------------------------------
%%% @author Yomi (baphled) Colledge <yomi@boodah.net>
%%% @copyright (C) 2009, Yomi (baphled) Colledge
%%% @doc
%%%
%%% Used to start Chatterl.
%%% To run from the shell
%%% <code>erl -s chatterl</code>
%%% Application used when we call Chatterl via:
%%% <application:start(chatterl).</code>
%%%
%%% @end
%%% Created :  3 Jan 2009 by Yomi (baphled) Colledge <yomi@boodah.net>
%%%-------------------------------------------------------------------
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
    case 'TopSupervisor':start_link(StartArgs) of
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
