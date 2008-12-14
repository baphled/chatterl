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

-export([start/0,shutdown/0,stop/1,handle_group/1]).
-export([create/1,user_connect/2,user_disconnect/1,list_users/0,list_groups/0]).

start() ->
    Pid = spawn(chatterl_groups, handle_group, [gb_trees:empty()]),
    case chatterl_serv:start() of
	{ok,_ServPid} ->
	    io:format("Starting Chatterl Group.~n");
	{error,{already_started,_ServPid}} ->
	    io:format("Serverl already started~n")
    end,
    erlang:register(?SERVER, Pid).

shutdown() ->
    ?SERVER ! shutdown.

stop(Group) ->
    ?SERVER ! {stop, Group}.

create(Group) ->
    ?SERVER ! {create, Group}.

user_connect(User,Group) ->
    ?SERVER ! {user_connect, User, Group}.

user_disconnect(User) ->
    ?SERVER ! {user_disconnect, User}.

list_users() ->
    ?SERVER ! {list_users}.

list_groups() ->
    ?SERVER ! chatterl_serv:view_groups().

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
	{stop, Group} ->
	    io:format("Shutting down ~p...~n", [Group]),
	    NewUsers = case chatterl_serv:drop(Group) of
		{ok, Result} -> io:format("~p~n", [Result]),
				List = gb_trees:to_list(Users),
				drop_user_from_group(Users,List,Group);
		{error, Error} -> io:format("Error:~p~n", [Error]),
				  Users
	    end,
	    handle_group(NewUsers);
	{user_connect, User, Group} ->
	    case chatterl_serv:group_exists(Group) of
		true -> 
		    case user_exists(User, Users) of
			true -> io:format("~p already connected to ~p~n",[User,Group]);
			false -> 
			    io:format("Connected user: ~p~n", [User]),
			    handle_group(gb_trees:insert(User, {User,Group}, Users))
		    end;
		false ->
		    io:format("Group doesn't exist ~p~n", [Group]);
		{error, Error} ->
		    io:format("Error: ~p~n", [Error])
	    end,
	    handle_group(Users);
	{user_disconnect, User} ->   
	    case user_exists(User, Users) of
		true -> handle_group(gb_trees:delete(User, Users)),
			io:format("Disconnected ~p~n", [User]);
		false -> io:format("~p is not connected.~n", [User])
	    end,
	    handle_group(Users);
	{list_users} ->
	    Results = gb_trees:keys(Users),
	    io:format("~p~n", [Results]),
	    handle_group(Users);
	{error, Error} ->
	    io:format("Error: ~p~n", [Error]),
	    handle_group(Users);
	error ->
	    io:format("Unknown error"),
	    handle_group(Users);
	shutdown ->
	    io:format("Shutting down...~n"),
	    drop_users(gb_trees:keys(Users), Users)
    end.

drop_user_from_group(UsersTree,[User|Users],Group) ->
    NewUsers = case gb_trees:lookup(User,UsersTree) of
	{value,Group} ->
	    io:format("Dropped ~p from ~p~n",[User, Group]),
	    gb_trees:delete(User, UsersTree);
        _ -> UsersTree	    
    end,
    drop_user_from_group(NewUsers,Users,Group);
drop_user_from_group(UsersTree,[],_Group) ->
    UsersTree.

drop_users([User|Users],UsersList) ->
    NewUsers = gb_trees:delete(User, UsersList),
    io:format("Delete: ~p~n", [User]),
    drop_users(Users, NewUsers);
drop_users([],UsersList) ->
    UsersList.

%% @private
user_exists(User,UsersTree) ->
    case gb_trees:is_defined(User, UsersTree) of
	true -> true;
	false -> false
    end.
user_exists(User, Group, UsersTree) ->
    case gb_trees:is_defined(User, UsersTree) of
	true ->
	    case gb_trees:lookup(User, UsersTree) of
		{value, Group} ->
		    true;
		_ -> false
	    end;
	false ->
	    false
    end.
