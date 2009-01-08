%% @author author <yomi@boodah.net>
%% @copyright 2009 Yomi Colledge.

%% @doc TEMPLATE.

-module(chatterl_web).
-author('author <yomi@boodah.net>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the chatterl_web server.
start() ->
    ensure_started(crypto),
    application:start(chatterl_web).

%% @spec stop() -> ok
%% @doc Stop the chatterl_web server.
stop() ->
    Res = application:stop(chatterl_web),
    application:stop(crypto),
    Res.
