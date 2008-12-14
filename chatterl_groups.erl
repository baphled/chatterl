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

-export([start/0,stop/0,connect/1,handle_group/1]).

start() ->
    Pid = spawn(chatterl_groups, handle_group, [dict:new()]),
    case chatterl_serv:start() of
	{ok, ServPid} ->
	    io:format("Starting chatterl_serv~n");
	{error,{_Msg,_ServPid}} ->
	    io:format("Serverl already started~n")
    end,
    erlang:register(?SERVER, Pid).

stop() ->
    ?SERVER ! shutdown.

connect(Group) ->
    ?SERVER ! {connect, Group}.

handle_group(Users) ->
    receive
	{connect, Group} ->
	    case chatterl_serv:create(Group, ?SERVER) of
		{ok, Msg} ->
		    io:format("Created group: ~p~n", [Msg]);
		{error, Error} ->
		    io:format("Error: ~p~n", [Error])
	    end,
	    handle_group(Users);
	shutdown ->
	    io:format("Shutting down...~n")
    end.
