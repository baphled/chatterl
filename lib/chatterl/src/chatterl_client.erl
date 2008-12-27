%%%-------------------------------------------------------------------
%%% File    : chatterl_client.erl
%%% Author  : Yomi Akindayini <yomi@boodah.net>
%%% Description : Erlang based chat client
%%%
%%% Created : 13 Dec 2008 by Yomi Akindayini <yomi@boodah.net>
%%%-------------------------------------------------------------------
-module(chatterl_client).

-export([login/1]).

login(User) ->
    case chatterl_serv:connect(User) of
	{error, Error} ->
	    io:format("~p~n", [Error]);
	{ok, Message} ->
	    io:format("~p is ~p.~n", [User, Message])
    end.
