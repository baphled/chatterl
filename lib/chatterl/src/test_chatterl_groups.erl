-module(test_chatterl_groups).

-include_lib("eunit/include/eunit.hrl").
-include_lib("chatterl.hrl").

-import(test_helpers,[start_client/3,start_group/2]).

%% Test our groups functionality
chatterl_group_handle_test_() ->
  {Client,Group,Description} = {"boodah","anuva","a nu room"},
  [{setup,
    fun() ->
        start_client(Client,Group,Description)
    end,
    fun(_) ->
        chatterl:stop() end,
   [{"Can retrieve group information",
     fun() ->
         ?assert(erlang:is_tuple(gen_server:call({global, chatterl_serv}, {get_group, Group}, infinity))),
         ?assertEqual(false,gen_server:call({global,Group},{user_exists,Client})),
         ?assertEqual({name,Group},gen_server:call({global,Group},name)),
         ?assertEqual({description,"a nu room"}, gen_server:call({global,Group},description)),
         ?assert(erlang:is_tuple(gen_server:call({global,Group},created))),
         ?assertEqual([],gen_server:call({global,Group},list_users)),
         ?assertEqual([],gen_server:call({global,Group},poll_messages))
     end},
    {"Client can join a group",
     fun() ->
         ?assertEqual({ok, "User added"},gen_server:call({global,Group},{join,Client})),
         ?assert(erlang:is_list(gen_server:call({global,Group},list_users))),
         ?assertEqual({error, "Already joined"},gen_server:call({global,Group},{join,Client})),
         ?assertEqual({error, "not connected"},gen_server:call({global,Group},{join,"nonUsers"})),
         ?assertEqual({error, "Invalid user name"},gen_server:call({global,Group},{join,{"nonUsers"}}))
     end},
    {"Client can leave a group",
     fun() ->
         ?assertEqual({ok, dropped}, gen_server:call({global,Group},{leave,Client})),
         ?assertEqual({error, "Not connected"}, gen_server:call({global,Group},{leave,Client})),
         ?assertEqual([],gen_server:call({global,Group},list_users))
     end},
    {"Client can send messages to a group",
     fun() ->
         ?assertEqual({ok, "User added"},gen_server:call({global,Group},{join,Client})),
         ?assertMatch({error, user_not_joined},gen_server:call({global,Group},{send_msg,"blah","hey"})),
         ?assertEqual({ok, msg_sent},gen_server:call({global,Group},{send_msg,Client,"hey"})),
         ?assertEqual({error,already_sent},gen_server:call({global,Group},{send_msg,Client,"heya"})),
         ?assert(erlang:is_list(gen_server:call({global,Group},poll_messages)))
     end}]}].
