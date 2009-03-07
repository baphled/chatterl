-module(chatterl_test).
-include_lib("eunit/include/eunit.hrl").

start_client(Client,Group,Description) ->
  {ok,Pid} = start_group(Group,Description),
  chatterl_client:start(Client),
  {ok,Pid}.

start_group(Group,Description) ->
  {ok, Pid} = chatterl_serv:start(),
  chatterl_groups:start(Group,Description),
  {ok,Pid}.

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
               register(chatterl_tests, Pid),
               Pid end,
    fun(P) ->
        exit(P, shutdown) end,
    [fun() ->
         Group = "nu",
         ?assertEqual({name,"nu"},gen_server:call({global,Group},name)),
         ?assertEqual({description,"a nu room"}, gen_server:call({global,Group},description)),
         ?assert(erlang:is_tuple(gen_server:call({global,Group},created))),
         ?assertEqual([],gen_server:call({global,Group},poll_messages)) end]}].
