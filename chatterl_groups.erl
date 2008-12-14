%%%-------------------------------------------------------------------
%%% File    : chatterl_groups.erl
%%% Author  : Yomi Akindayini <yomi@boodah.net>
%%% Description : Handle chatterl's group system
%%%
%%% Created : 14 Dec 2008 by Yomi Akindayini <yomi@boodah.net>
%%%-------------------------------------------------------------------
-module(chatterl_groups).

-define(SERVER, chatterl_groups).
-define(CHATTERL, chatterl_serv).

-export([start/0,connect/1,loop/1]).

start() ->
    Pid = spawn(chatterl_groups, loop, [dict:new()]),
    case chatterl_serv:start() of
	{ok, ServPid} ->
	    io:format("Starting chatterl_serv~n");
	{error,{_Msg,_ServPid}} ->
	    io:format("Serverl already started~n")
    end,
    erlang:register(?SERVER, Pid).

connect(Group) ->
    ?SERVER ! {connect, Group}.

loop(Users) ->
    receive
	{connect, Group} ->
	    chatterl_serv:create(Group, ?SERVER),
	    loop(Users)
    end.
