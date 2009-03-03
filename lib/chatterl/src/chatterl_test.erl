-module(chatterl_test).
-include_lib("eunit/include/eunit.hrl").

%% Test Chatterl server.
%%
%% Todo Ideally we need to match the PID
chatterl_serv_test_() ->
  [?_assert({error,{already_started,<<"0.4711.0">>}} /= chatterl_serv:start())].

%% Test that our groups function as expected.
%% Instead of testing that the group can be created we'll create and group drops on setup.
chatterl_group_info_test_() ->
  chatterl_groups:start("nu","a new room"),
  [?_assertEqual({name,"nu"}, gen_server:call({global,"nu"},name)),
   ?_assertEqual({description,"a new room"}, gen_server:call({global,"nu"},description)),
   ?_assert(erlang:is_tuple(gen_server:call({global,"nu"},created))),
   ?_assertEqual([], gen_server:call({global,"nu"},poll_messages))].

%% Test that a client can connect to a group.
chatterl_client_can_join_groups_test_() ->
  chatterl_serv:start(),
  chatterl_groups:start("nu","a new room"),
  chatterl_client:start("baft"),
  gen_server:call({global,"nu"},{join,"baft"}),
  [?_assert(erlang:is_list(gen_server:call({global,"nu"},list_users))),
   ?_assertEqual({error, "Already joined"},gen_server:call({global,"nu"},{join,"baft"})),
   ?_assertEqual({error, "not connected"},gen_server:call({global,"nu"},{join,"nonUsers"})),
   ?_assertEqual({error, "Invalid user name"},gen_server:call({global,"nu"},{join,{"nonUsers"}}))].

%% Test that our clients can connect to chatterl_serv & interact with Chatterl as we expect.
chatterl_client_test_() ->
  [?_assert({error,{already_started,<<"0.4711.0">>}} /= chatterl_client:start("bobby")),
  ?_assertMatch({error, "Unable to send message"},gen_server:call({global,"bobby"},{group_msg,"blah","hey"}))].
