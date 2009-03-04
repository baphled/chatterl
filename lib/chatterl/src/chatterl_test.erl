-module(chatterl_test).
-include_lib("eunit/include/eunit.hrl").


%% Helper function to help create a client.
start_client(Client,Group,Description) ->
  chatterl_serv:start(),
  chatterl_groups:start(Group,Description),
  chatterl_client:start(Client).

%% Test Chatterl server.
%%
%% Todo Ideally we need to match the PID
chatterl_serv_test_() ->
  [?_assert({error,{already_started,<<"0.4711.0">>}} /= chatterl_serv:start())].

%% Test that our groups function as expected.
%% Instead of testing that the group can be created we'll create and group drops on setup.
chatterl_group_info_test_() ->
  chatterl_groups:start("test","a room"),
  [?_assertEqual({name,"test"}, gen_server:call({global,"test"},name)),
   ?_assertEqual({description,"a room"}, gen_server:call({global,"test"},description)),
   ?_assert(erlang:is_tuple(gen_server:call({global,"test"},created))),
   ?_assertEqual([], gen_server:call({global,"test"},poll_messages))].

%% Test that a client can connect to a group.
chatterl_client_can_join_groups_test_() ->
  Client = "noob",
  Group = "anuva",
  start_client(Client,Group,"anuva room"),
  {timeout, 23,
   [?_assertEqual([],gen_server:call({global,Group},list_users)),
   ?_assertEqual({ok, "User added"},gen_server:call({global,Group},{join,Client})),
   ?_assert(erlang:is_list(gen_server:call({global,Group},list_users))),
   ?_assertEqual({error, "Already joined"},gen_server:call({global,Group},{join,"noob"})),
   ?_assertEqual({error, "not connected"},gen_server:call({global,Group},{join,"nonUsers"})),
   ?_assertEqual({error, "Invalid user name"},gen_server:call({global,Group},{join,{"nonUsers"}}))]}.

chatterl_client_can_leave_groups_test_() ->
  Client = "baft",
  Group = "nu",
  start_client(Client,Group,"a nu room"),
  [?_assertEqual({ok, "User added"},gen_server:call({global,Group},{join,Client})),
   ?_assert(erlang:is_list(gen_server:call({global,Group},list_users))),
   ?_assertEqual({error, "Not connected"},gen_server:call({global,Group},{leave,"not a user"})),
   ?_assertEqual({ok, dropped},gen_server:call({global,Group},{leave,Client}))].

chatterl_client_can_send_messages_test_() ->
  Client = "noobie",
  Group = "nu2",
  start_client(Client,Group,"yet another room"),
  [?_assertEqual({error, user_not_joined},gen_server:call({global,Group},{send_msg,Client,"hey all"})),
   ?_assertEqual({ok, "User added"},gen_server:call({global,Group},{join,Client})),
   ?_assertEqual({ok, msg_sent},gen_server:call({global,Group},{send_msg,Client,"wassup"})),
   ?_assertEqual({error, already_sent},gen_server:call({global,Group},{send_msg,Client,"wassup"})),
   ?_assertEqual({error,"Cannot find user!"}, gen_server:call({global,Client},{private_msg,"jim","wassup"})),
   ?_assert(erlang:is_list(gen_server:call({global,Client},poll_messages)))].

%% Test that our clients can connect to chatterl_serv & interact with Chatterl as we expect.
chatterl_client_test_() ->
  [?_assert(erlang:is_tuple(chatterl_client:start("bobby"))),
   ?_assertMatch({error, "Unable to send message"},gen_server:call({global,"bobby"},{group_msg,"blah","hey"}))].
