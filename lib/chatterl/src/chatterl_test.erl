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

starter() ->
  {ok, Pid} = chatterl_serv:start(),
  chatterl_groups:start("nu","a nu room"),
  {ok,Pid}.

chatterl_group_info_test_() ->
  [{setup, fun() ->
               {ok, Pid} = starter(),
               register(natterp, Pid),
               Pid end,
    fun(P) ->
        exit(P, shutdown) end,
    [fun() ->
         ?assertEqual({name,"nu"},gen_server:call({global,"nu"},name)) end]}].
