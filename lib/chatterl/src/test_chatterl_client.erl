%%%----------------------------------------------------------------
%%% @author Yomi Colledge <yomi@boodah.net>
%%% @doc
%%% Test cases for chatterl_client
%%%
%%% @end
%%% @copyright 2009 Yomi Colledge
%%%----------------------------------------------------------------
-module(test_chatterl_client).

-include_lib("eunit/include/eunit.hrl").
-include_lib("chatterl.hrl").

-import(test_helpers,[start_client/3,start_group/2]).

chatterl_client_handle_test_() ->
  {Client1,Client2,Group,Description} = {"noobie","blah","anuva1","a nu room"},
  [{setup,
    fun() ->
        start_client(Client1,Group,Description),
        chatterl_client:start(Client2)
    end,
    fun(_) ->
        mnesia:delete_table(registered_user),
        chatterl:stop() end,
    [{"Can we retrieve client thier name",
      fun() ->
          ?assertEqual({name,Client1}, gen_server:call({global,Client1},client_name))
			end},
		{"Client can get a list of groups on chatterl",
			fun() ->
          ?assert(erlang:is_list(gen_server:call({global,Client1},groups)))
			end},
		{"Client can get an empty list of the groups they are joined to",
			fun() ->
          ?assertEqual([], gen_server:call({global,Client2},groups))
			end},
		{"Client can poll for empty messages",
			fun() ->
          ?assertEqual([], gen_server:call({global,Client2},poll_messages))
      end},
      {"Can not join a non-existent group",
       fun() ->
          ?assertEqual({error,"Unknown error!"}, gen_server:call({global,Client2},{join_group,"none"}))
				end},
			{"Client can join a real group",
				fun() ->
          ?assertEqual({ok,joined_group}, gen_server:call({global,Client1},{join_group,Group}))
				end},
		{"Client can list the groups they are joined to",
			fun() ->
          ?assertEqual(["anuva1"],gen_server:call({global,Client1},groups))
      end},
     {"Can a client send private messages",
      fun() ->
          ?assertEqual({ok,msg_sent}, gen_server:call({global,Client1},{private_msg,Client2,"sup"}))
			end},
		{"Client can not send a message to another client that does not exist",
			fun() ->
          ?assertEqual({fail,"Cannot find user!"}, gen_server:call({global,Client1},{private_msg,"noob","sup"}))
			end},
		{"Client can send a message to a client that is registered but not logged in",
			fun() ->
          ?assertEqual({ok,msg_sent}, chatterl_client:private_msg(Client2,Client1,"sup"))
			end},
		{"Client can retrieve a list of messages",
			fun() ->
          ?assert(erlang:is_list(gen_server:call({global,Client1},poll_messages)))
			end},
		{"Client can not send a message to their self",
			fun() ->
          ?assertEqual({error,"Can not send to self!"}, gen_server:call({global,Client1},{private_msg,Client1,"sup"}))
     end},
     {"Client can not leave a group they are not joined to",
      fun() ->
          ?assertEqual({error,"Not connected"}, gen_server:call({global,Client2},{leave_group,Group}))
			end},
		{"A client can leave a group",
			fun() ->
          ?assertEqual({ok, drop_group}, gen_server:call({global,Client1},{leave_group,Group}))
			end},
		{"A client process can be stopped",
			fun() ->
          ?assertEqual(stopped,gen_server:call({global,Client2},{stop,"because"}))
      end}]}].
