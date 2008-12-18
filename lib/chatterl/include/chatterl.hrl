%%%-------------------------------------------------------------------
%%% File    : chatterl.hrl
%%% Author  : Yomi Akindayini <yomi@boodah.net>
%%% Description : Records associated with Chatterl
%%%
%%% Created : 18 Dec 2008 by Yomi Akindayini <yomi@boodah.net>
%%%-------------------------------------------------------------------

%% Records for chatterl_serv, will track the total users & groups.
-record(chatterl, {groups, users}).
%% Records associated with groups
-record(groups, {name, description, users}).
%% Records associated with users
-record(users, {name, ip}).
