%%%-------------------------------------------------------------------
%%% File    : chatterl_client.erl
%%% Author  : Yomi Akindayini <yomi@boodah.net>
%%% Description : Erlang based chat client
%%%
%%% Created : 13 Dec 2008 by Yomi Akindayini <yomi@boodah.net>
%%%-------------------------------------------------------------------
-module(chatterl_client).

-export([start/0,login/2]).

start() ->
    {ok, connected}.

login(User,Pass) ->
    {error, not_valid}.
