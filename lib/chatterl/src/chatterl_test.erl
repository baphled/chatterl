-module(chatterl_test).
-include_lib("eunit/include/eunit.hrl").

%% Test that our groups function as expected.
%% Instead of testing that the group can be created we'll create and group drops on setup.
chatterl_serv_start_test_() ->
   [?_assert(erlang:is_tuple(chatterl_serv:start()))].

chatterl_serv_stop_test_() ->
  chatterl_serv:start(),
  [?_assert(erlang:is_tuple(chatterl_serv:create("nu","nu room"))),
   ?_assertEqual(stopped,chatterl_serv:stop())].

chatterl_group_info_test_() ->
  [{setup, fun() ->
               {Group,Description} = {"nu","a nu room"},
               {ok, Pid} = start_group(Group,Description),
               register(chatterl_groups_tests, Pid),
               Pid end,
    fun(_) ->
        chatterl_serv:stop() end,
    [fun() ->
         Group = "nu",
         ?assertEqual({name,"nu"},gen_server:call({global,Group},name)),
         ?assertEqual({description,"a nu room"}, gen_server:call({global,Group},description)),
         ?assert(erlang:is_tuple(gen_server:call({global,Group},created))),
         ?assertEqual([],gen_server:call({global,Group},poll_messages)) end]}].

%% Test that a client can connect to a group.
chatterl_client_can_join_groups_test_() ->
  [{setup, fun() ->
               {Client,Group,Description} = {"noob","nu","a nu room"},
               {ok, Pid} = start_client(Client,Group,Description),
               register(chatterl_client_join_tests, Pid),
               Pid end,
    fun(_) ->
        chatterl_serv:stop() end,
   [fun() ->
        {Client,Group} = {"noob","nu"},
        ?assertEqual([],gen_server:call({global,Group},list_users)),
        ?assertEqual([],gen_server:call({global,Group},poll_messages)),
        ?assertEqual({ok, "User added"},gen_server:call({global,Group},{join,Client})),
        ?assert(erlang:is_list(gen_server:call({global,Group},list_users))),
        ?assertEqual({error, "Already joined"},gen_server:call({global,Group},{join,Client})),
        ?assertEqual({error, "not connected"},gen_server:call({global,Group},{join,"nonUsers"})),
        ?assertEqual({error, "Invalid user name"},gen_server:call({global,Group},{join,{"nonUsers"}})),
        ?assertEqual({ok, dropped}, gen_server:call({global,Group},{leave,Client})),
        ?assertEqual({error, "Not connected"}, gen_server:call({global,Group},{leave,Client})),
        ?assertEqual([],gen_server:call({global,Group},list_users)),
        ?assertEqual({ok, "User added"},gen_server:call({global,Group},{join,Client})),
        ?assertMatch({error, user_not_joined},gen_server:call({global,Group},{send_msg,"blah","hey"})),
        ?assertEqual({ok, msg_sent},gen_server:call({global,Group},{send_msg,Client,"hey"})),
        ?assert(erlang:is_list(gen_server:call({global,Group},poll_messages)))
        end]}].


%% Helper functions.
start_client(Client,Group,Description) ->
  {ok,Pid} = start_group(Group,Description),
  chatterl_client:start(Client),
  {ok,Pid}.

start_group(Group,Description) ->
  {ok, Pid} = chatterl_serv:start(),
  chatterl_groups:start(Group,Description),
  {ok,Pid}.
