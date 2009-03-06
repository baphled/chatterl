-module(chatterl_test).
-include_lib("eunit/include/eunit.hrl").


%% Helper function to help create a client.
start_client(Client,Group,Description) ->
  chatterl_serv:start(),
  chatterl_groups:start(Group,Description),
  chatterl_client:start(Client).

%% Test that our groups function as expected.
%% Instead of testing that the group can be created we'll create and group drops on setup.
chatterl_group_info_test_() ->
  {setup,
   fun() -> start_client("blah","test","a room") end,
   fun(_Blah) -> chatterl_serv:stop() end,
   fun test_group_info/0}.

test_group_info() ->
  [?_assertEqual({name,"test"}, gen_server:call({global,"test"},name)),
   ?_assertEqual({description,"a room"}, gen_server:call({global,"test"},description)),
   ?_assert(erlang:is_tuple(gen_server:call({global,"test"},created))),
   ?_assertEqual([], gen_server:call({global,"test"},poll_messages))].

%% Test that a client can connect to a group.
chatterl_client_can_join_groups_test_() ->
  {setup,
   fun() -> start_client("noob","anuva","a room") end,
   fun(_) -> chatter_serv:stop() end,
   fun test_client_can_join_groups/0}.

test_client_can_join_groups() ->
  Client = "noob",
  Group = "anuva",
   [?_assertEqual([],gen_server:call({global,Group},list_users)),
   ?_assertEqual({ok, "User added"},gen_server:call({global,Group},{join,Client})),
   ?_assert(erlang:is_list(gen_server:call({global,Group},list_users))),
   ?_assertEqual({error, "Already joined"},gen_server:call({global,Group},{join,"noob"})),
   ?_assertEqual({error, "not connected"},gen_server:call({global,Group},{join,"nonUsers"})),
   ?_assertEqual({error, "Invalid user name"},gen_server:call({global,Group},{join,{"nonUsers"}}))].

chatterl_client_can_leave_groups_test_() ->
  {setup,
   fun() -> start_client("noob","anuva","a room") end,
   fun(_) -> chatter_serv:stop() end,
   fun test_client_can_leave_groups/0}.

generate_setup(Fun) ->
  {setup,
   fun() -> start_client("noob","anuva","a room") end,
   fun(_) -> chatter_serv:stop() end,
   Fun}.

test_client_can_leave_groups() ->
  Client = "baft",
  Group = "nu",
  [?_assertEqual({ok, "User added"},gen_server:call({global,Group},{join,Client})),
   ?_assert(erlang:is_list(gen_server:call({global,Group},list_users))),
   ?_assertEqual({error, "Not connected"},gen_server:call({global,Group},{leave,"not a user"})),
   ?_assertEqual({ok, dropped},gen_server:call({global,Group},{leave,Client}))].

chatterl_client_can_send_messages_test_() ->
  generate_setup(fun client_can_send_messages/0).

client_can_send_messages() ->
  {Group,Client} = {"anuva","noob"},
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
