-module(chatterl_test).
-include_lib("eunit/include/eunit.hrl").

%% Test Chatterl server.
%%
%% Todo Ideally we need to match the PID
chatterl_serv_test_() ->
  [?_assert({error,{already_started,<<"0.4711.0">>}} /= chatterl_serv:start())].

%% Test that our groups function as expected.
%% Instead of testing that the group can be created we'll create and group drops on setup.
generate_group_tests_() ->
  chatterl_groups:start("nu","a new room"),
  [?assertEqual("nu", gen_server:call({global,"nu"},name)),
  ?assertEqual("a new room", gen_server:call({global,"nu"},description)),
  ?assert(erlang:is_tuple(gen_server:call({global,"nu"},created)))].

%% Test that our clients can connect to chatterl_serv & interact with Chatterl as we expect.
chatterl_client_test_() ->
  [?_assert({error,{already_started,<<"0.4711.0">>}} /= chatterl_client:start("bobby")),
  ?_assertMatch({error, "Unable to send message"},gen_server:call({global,"bobby"},{group_msg,"blah","hey"}))].
