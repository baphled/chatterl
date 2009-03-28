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
    [fun() ->
         {Group1,Description} = {"anuva","nu room"},
         ?assertEqual([],chatterl_serv:list_groups()),
         ?assertEqual([],chatterl_serv:list_users()),
         ?assertEqual({error,"Group doesn't exist!"},chatterl_serv:list_users(Group1)),
         ?assert(erlang:is_tuple(chatterl_serv:create(Group1,Description))),
         ?assertEqual({error,already_created},chatterl_serv:create(Group1,Description)),
         ?assertEqual([],chatterl_serv:list_users(Group1)),
         ?assertEqual([Group1],chatterl_serv:list_groups())
      end]}].

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

chatterl_serv_register_test_() ->
  {Nick1,Name1,Email1,Password1} = {"noobie","noobie 1","noobie@noobie.com","blahblah"},
  [{setup,
    fun() ->
        chatterl:start()
    end,
    fun(_) ->
        chatterl:stop(),
        mnesia:delete_table(registered_user)
    end,
    [{"Client can login via chatterl_serv",
      fun() ->
          ?assertEqual({error,"blah's passwords must match"},chatterl_serv:register("blah",{"yo","y@me.com","pass","pas"})),
          ?assertEqual({ok,"noobie is registered"},chatterl_serv:register(Nick1,{Name1,Email1,Password1,Password1})),
          ?assertEqual({error,"noobie is already registered"},chatterl_serv:register(Nick1,{Name1,Email1,Password1,Password1}))
      end}]}].

chatterl_registered_users_can_login_and_out_test_() ->
  {Nick1,Name1,Email1,Password1,Nick2,Password2} = {"noobie","noobie 1","noobie@noobie.com","blahblah","nerf","asdasd"},
  [{setup,
    fun() ->
        chatterl:start(),
        chatterl_store:register(Nick1,{Name1,Email1,Password1,Password1})
    end,
    fun(_) ->
        mnesia:delete_table(registered_user),
        chatterl:stop()
    end,
    [{"Can our client login using chatterl_serv?",
     fun() ->
         ?assertEqual({error,"Unable to login"},chatterl_serv:login(Nick1,"blah")),
         ?assertEqual({error,"Not registered"},chatterl_serv:login(Nick2,Password2)),
         ?assertEqual({ok,"noobie is logged in."},chatterl_serv:login(Nick1,Password1)),
         ?assertEqual([Nick1],chatterl_store:get_logged_in())
     end},
    {"Does chatterl handle logins as we expect them to",
     fun() ->
         ?assertEqual(true,gen_server:call({global,chatterl_serv},{user_exists,Nick1})),
         ?assertEqual(false,gen_server:call({global,chatterl_serv},{user_exists,Nick2})),
         ?assertEqual(true,chatterl_store:logged_in(Nick1)),
         ?assertEqual({ok,"noobie is logged out."},chatterl_serv:logout(Nick1)),
         ?assertEqual(false,gen_server:call({global,chatterl_serv},{user_exists,Nick1}))
     end},
    {"Can a client successfully logout",
      fun() ->
          ?assertEqual(false,chatterl_store:logged_in(Nick2)),
          ?assertEqual({error,"Not logged in"},chatterl_serv:logout(Nick2)),
          ?assertEqual(false,chatterl_store:logged_in(Nick1))
      end}]}].
