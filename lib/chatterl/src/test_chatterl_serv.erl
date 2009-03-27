-module(test_chatterl_serv).

-include_lib("eunit/include/eunit.hrl").
-include_lib("chatterl.hrl").

chatterl_serv_test_() ->
  [{setup,
    fun() ->
        ?assertEqual(ok,chatterl:start())
    end,
    fun(_) ->
        chatterl:stop() end,
    [{timeout, 5000,
      fun() ->
         {Group1,Description} = {"anuva","nu room"},
         ?assertEqual([],chatterl_serv:list_groups()),
         ?assertEqual([],chatterl_serv:list_users()),
         ?assertEqual({error,"Group doesn't exist!"},chatterl_serv:list_users(Group1)),
         ?assert(erlang:is_tuple(chatterl_serv:create(Group1,Description))),
         ?assertEqual({error,already_created},chatterl_serv:create(Group1,Description)),
         ?assertEqual([],chatterl_serv:list_users(Group1)),
         ?assertEqual([Group1],chatterl_serv:list_groups())
      end}]}].

chatterl_serv_groups_test_() ->
  {Client1,Group1,Group2,Description} = {"blah","room","anuva","nu room"},
  [{setup,
    fun() ->
        chatterl:start(),
        chatterl_serv:create(Group1,Description),
        chatterl_serv:create(Group2,"anuva room")
    end,
    fun(_) ->
        chatterl:stop() end,
    [fun() ->
         ?assertEqual([Group2,Group1],chatterl_serv:list_groups()),
         ?assertEqual({ok,"connected"},chatterl_serv:connect(Client1)),
         ?assertEqual([Client1],chatterl_serv:list_users()),
         ?assertEqual({error,"blah is unable to connect."},chatterl_serv:connect(Client1)),
         ?assertEqual({error,"Unable to disconnect noone"},chatterl_serv:disconnect("noone")),
         ?assertEqual({ok,"User disconnected: blah"},chatterl_serv:disconnect(Client1)),
         ?assertEqual({error,"Can not find group."},chatterl_serv:group_description("anuva1")),
         ?assertEqual({description,Description},chatterl_serv:group_description(Group1)),
         ?assert(erlang:is_tuple(gen_server:call({global, chatterl_serv}, {get_group, Group1}))),
         ?assertEqual({error,"Can not find blah"},chatterl_serv:drop("blah")),
         ?assertEqual({ok,"Group dropped anuva"},chatterl_serv:drop(Group2)),
         ?assert(erlang:is_list(gen_server:call({global,chatterl_serv},{group_info,Group1})))
     end]}].
