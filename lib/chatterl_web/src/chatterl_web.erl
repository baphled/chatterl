%% @author author <yomi@boodah.net>
%% @copyright 2009 Yomi Colledge.

%% @doc Callbacks for the chatterl_web application.

-module(chatterl_web).
-author('author <yomi@boodah.net>').

-behaviour(application).
-export([start/0,start/2,stop/1]).

%% @spec start() -> ok
%% @doc Start the chatterl_web server.
start() ->
    crypto:start(),
    mochiweb:start(),
    chatterl_web_deps:ensure(),
    application:start(?MODULE).

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for chatterl_web.
start(_Type, _StartArgs) ->
    chatterl_web_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for chatterl_web.
stop(_State) ->
    Res = chatterl_web_sup:stop(),
    Res.
