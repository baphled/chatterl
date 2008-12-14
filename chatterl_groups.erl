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

-export([start/0,stop/0]).
-export([register_nick/2,create/1,handle_group/1,user_exists/2]).

start() ->
    Pid = spawn(chatterl_groups, handle_group, [gb_trees:empty()]),
    case chatterl_serv:start() of
	{ok,ServPid} ->
	    io:format("Starting Chatterl Group.");
	{error,{already_started,ServPid}} ->
	    io:format("Serverl already started~n")
    end,
    erlang:register(?SERVER, Pid).

stop() ->
    ?SERVER ! shutdown.

create(Group) ->
    ?SERVER ! {create, Group}.

register_nick(User,Group) ->
    ?SERVER ! {register, User, Group}.

handle_group(Users) ->
    receive
	{create, Group} ->
	    Message = case chatterl_serv:create(Group, ?SERVER) of
		{ok, GroupName} ->
		    "Created group: "++GroupName;
		{error, Error} ->
		    "Error: " ++ Error
	    end,
	    io:format("~p~n", [Message]),
	    handle_group(Users);
	{register, User, Group} ->
	    case chatterl_serv:group_exists(Group) of
		true -> 
		    case user_exists(User, Users) of
			true -> io:format("~p already registered~n",[User]);
			false -> 
			    io:format("Added user: ~p~n", [User]),
			    handle_group(gb_trees:insert(User, {User,Group}, Users))
		    end;
		false ->
		    io:format("Group doesn't exist ~p~n", [Group]);
		{error, Error} ->
		    io:format("Error: ~p~n", [Error])
	    end,
	    handle_group(Users);
	{error, Error} ->
	    io:format("Error: ~p~n", [Error]),
	    handle_group(Users);
	error ->
	    io:format("Unknown error");
	shutdown ->
	    io:format("Shutting down...~n")
    end.

user_exists(User, Users) ->
    case gb_trees:is_defined(User, Users) of
	true ->
	    true;
	false ->
	    false
    end.
