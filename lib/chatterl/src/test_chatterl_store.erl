-module(test_chatterl_store).

-include_lib("eunit/include/eunit.hrl").
-include_lib("chatterl.hrl").

-import(test_helpers,[check_json/1]).
% Basic units to implement the storage client and group processe states
chatterl_store_test_() ->
  {Client,Group,Description} = {"noob","nu","nu room"},
  [{setup,
    fun() ->
        chatterl:start(),
        chatterl_serv:create(Group,Description),
        chatterl_client:start(Client),
        gen_server:call({global,Client},{join_group,Group})
    end,
    fun(_) ->
        mnesia:clear_table(group),
        mnesia:clear_table(client),
        mnesia:clear_table(registered_user),
        chatterl:stop()
    end,
    [{timeout, 5000,
      fun() ->
          ?assertEqual([registered_user,messages,client,group,schema],mnesia:system_info(tables))
      end},
    fun() ->
        GroupState = gen_server:call({global,Group},get_state),
        ?assertEqual(Group,GroupState#group.name),
        ?assertEqual(Description,GroupState#group.description),
        ?assert(erlang:is_tuple(GroupState#group.created)),
        ?assertEqual({0,nil},GroupState#group.messages),
        ?assert(erlang:is_tuple(GroupState#group.users)),
        ?assertEqual({error,"Group doesn't exist"},chatterl_store:group("anuva")),
        ?assertEqual(ok,chatterl_store:group(Group)),
        ?assertEqual([GroupState],chatterl_store:get_group(Group))
    end,
    fun() ->
        ClientState = gen_server:call({global,Client},get_state),
        ?assertEqual(Client,ClientState#client.name),
        ?assertEqual(ok,chatterl_store:user(Client)),
        ?assertEqual({0,nil},ClientState#client.messages),
        ?assertEqual({error,"User doesn't exist"},chatterl_store:user("blah")),
        ?assert(erlang:is_tuple(ClientState#client.groups)),
        ?assertEqual([ClientState],chatterl_store:get_user(Client))
       end]}].

chatterl_store_user_register_test_() ->
  {Nick1,Name1,Email1,Password1} = {"noobie","noobie 1","noobie@noobie.com","blahblah"},
  {Nick2,Name2,Email2,Password2} = {"nerf","nerf 1","nerf@noobie.com","asfdasdf"},
  [{setup,
    fun() ->
        chatterl:start(),
        chatterl_client:start(Nick1)
    end,
    fun(_) ->
        mnesia:delete_table(client),
        mnesia:delete_table(group),
        mnesia:clear_table(registered_user),
        chatterl:stop()
    end,
    [fun() ->
          ?assertEqual({ok,"noobie is registered"},chatterl_store:register(Nick1,{Name1,Email1,Password1,Password1})),
          ?assertEqual({ok,"nerf is registered"},chatterl_store:register(Nick2,{Name2,Email2,Password2,Password2})),
          ?assertEqual({error,"noobie's passwords must match"},chatterl_store:register(Nick1,{Name1,Email1,Password1,"blah"}))
     end,
     fun() ->
         ?assertEqual({ok,"noobie Authorized"},chatterl_store:auth(Nick1,Password1)),
         ?assertEqual({error,"Unable to authorise blah"},chatterl_store:auth("blah","blah")),
         ?assertEqual([{Nick2,Name2,Email2},{Nick1,Name1,Email1}], chatterl_store:registered())
     end,
     fun() ->
         ?assertEqual({error,"noobie is already registered"},chatterl_store:register(Nick1,{Name1,Email1,Password1,Password1})),
         ?assert(chatterl_store:is_auth(Nick1,Password1)),
         ?assert(false =:= chatterl_store:is_auth(Nick1,"blah"))
      end]}].

chatterl_registered_user_store_group_test_() ->
  ContentType = ["text/json"],
  {Nick1,Name1,Email1,Password1} = {"noobie","noobie 1","noobie@noobie.com","blahblah"},
  {Nick2,Name2,Email2,Password2} = {"nerf","nerf 1","nerf@noobie.com","asfdasdf"},
  [{setup,
    fun() ->
        chatterl:start()
    end,
    fun(_) ->
        mnesia:clear_table(registered_user),
        chatterl:stop()
    end,
    [{timeout,5000,
      fun() ->
          ?assertEqual({ok,"noobie is registered"},chatterl_store:register(Nick1,{Name1,Email1,Password1,Password1})),
          ?assertEqual([{Nick1,Name1,Email1}],chatterl_store:registered()),
          ?assertEqual({struct,[{<<"registered">>,
                                 {struct,[{<<"client">>,[{struct,[{<<"nick">>,<<"noobie">>}]},
                                                         {struct,[{<<"name">>,<<"noobie 1">>}]},
                                                         {struct,[{<<"email">>,<<"noobie@noobie.com">>}]}]}]}
                                }]},
                       check_json(mochijson2:decode(chatterl_mid_man:registered_list(ContentType))))
      end},
     fun() ->
         ?assertEqual({ok,"nerf is registered"},chatterl_store:register(Nick2,{Name2,Email2,Password2,Password2})),
         ?assertEqual([{Nick2,Name2,Email2},{Nick1,Name1,Email1}],chatterl_store:registered()),
         ?assertEqual({struct,[{<<"registered">>,[
                                                  {struct,[{<<"client">>,[{struct,[{<<"nick">>,<<"nerf">>}]},
                                                                          {struct,[{<<"name">>,<<"nerf 1">>}]},
                                                                          {struct,[{<<"email">>,<<"nerf@noobie.com">>}]}]}]},
                                                  {struct,[{<<"client">>,[{struct,[{<<"nick">>,<<"noobie">>}]},
                                                                          {struct,[{<<"name">>,<<"noobie 1">>}]},
                                                                          {struct,[{<<"email">>,<<"noobie@noobie.com">>}]}]}]}
                                                 ]}]}, check_json(mochijson2:decode(chatterl_mid_man:registered_list(ContentType))))
     end]}].

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
    [{"Do registered client login successfully & unregistered fail",
      fun() ->
          ?assertEqual([],chatterl_store:get_registered(Nick2)),
          ?assertEqual([{registered_user,Nick1,Name1,Email1,erlang:md5(Password1),0}],chatterl_store:get_registered(Nick1)),
          ?assertEqual({error,"Unable to login"},chatterl_store:login(Nick1,"blah")),
          ?assertEqual({error,"Not Registered"},chatterl_store:login(Nick2,Password2)),
          ?assertEqual({ok,"Logged in"},chatterl_store:login(Nick1,Password1)),
          ?assertEqual([{registered_user,Nick1,Name1,Email1,erlang:md5(Password1),1}],chatterl_store:get_registered(Nick1)),
          ?assertEqual([Nick1],chatterl_store:get_logged_in())
      end},
     {"What happens when a registered client & an unregistered client try to log out",
       fun() ->
         ?assertEqual(false,chatterl_store:logged_in(Nick2)),
         ?assertEqual({error,"Not logged in"},chatterl_store:logout(Nick2)),
         ?assertEqual(true,chatterl_store:logged_in(Nick1)),
         ?assertEqual([{Nick1,Name1,Email1}],chatterl_store:registered()),
         ?assertEqual({ok,"Logged out"},chatterl_store:logout(Nick1)),
         ?assertEqual(false,chatterl_store:logged_in(Nick1))
     end}]}].

chatterl_registered_users_can_logout_properly_test_() ->
  {Nick1,Name1,Email1,Password1,Nick2} = {"noobie","noobie 1","noobie@noobie.com","blahblah","nerf"},
  {NewName,NewEmail,NewPassword} = {"new name","y@me.com","encrypt"},
  [{setup,
    fun() ->
        chatterl:start(),
        chatterl_store:register(Nick1,{Name1,Email1,Password1,Password1}),
        chatterl_serv:login(Nick1,Password1)
    end,
    fun(_) ->
        mnesia:delete_table(registered_user),
        chatterl:stop()
    end,
    [{"Client is able to edit their profiles",
      fun() ->
          ?assertEqual({error,"nerf not logged in"},chatterl_store:edit_profile(Nick2,{firstname,"some name"})),
          ?assertEqual({ok,"Updated profile"},chatterl_store:edit_profile(Nick1,{firstname,NewName})),
          ?assertEqual({error,"Unable to update profile"},chatterl_store:edit_profile(Nick1,{blah,NewName})),
          ?assertEqual([{registered_user,Nick1,NewName,Email1,erlang:md5(Password1),1}],chatterl_store:get_registered(Nick1)),
          ?assertEqual({ok,"Updated profile"},chatterl_store:edit_profile(Nick1,{email,NewEmail})),
          ?assertEqual([{registered_user,Nick1,NewName,NewEmail,erlang:md5(Password1),1}],chatterl_store:get_registered(Nick1)),
          ?assertEqual({ok,"Updated profile"},chatterl_store:edit_profile(Nick1,{password,NewPassword})),
          ?assertEqual([{registered_user,Nick1,NewName,NewEmail,erlang:md5(NewPassword),1}],chatterl_store:get_registered(Nick1))
      end},
     {"Client processes log their selves out on termination",
      fun() ->
          ?assertEqual(true,chatterl_store:logged_in(Nick1)),
          ?assertEqual({ok,"Logged out"},chatterl_client:stop(Nick1)),
          ?assertEqual(false,chatterl_store:logged_in(Nick1)),
          ?assertEqual({ok,"noobie is logged in."},chatterl_serv:login(Nick1,NewPassword))
      end}]}].

chatterl_registered_users_archive_messages_test_() ->
  CreatedOn1 = erlang:localtime(),
  {Nick1,Name1,Email1,Password1} = {"noobie","noobie 1","noobie@noobie.com","blahblah"},
  {Nick2,Name2,Email2,Password2} = {"nerf","nerf 1","nerf@noobie.com","asfdasdf"},
  [{setup,
    fun() ->
        chatterl:start(),
        chatterl_store:register(Nick1,{Name1,Email1,Password1,Password1}),
        chatterl_store:register(Nick2,{Name2,Email2,Password2,Password2}),
        chatterl_serv:login(Nick2,Password2)
    end,
    fun(_) ->
        chatterl:stop(),
        mnesia:delete_table(registered_user),
        mnesia:delete_table(messages)
    end,
    [{timeout,50000,{"Client send archived messages as expected",
      fun() ->
          ?assertEqual({error,lists:append(Nick1," not logged in")},chatterl_store:archive_msg(Nick1,{erlang:localtime(),Nick2,"hey"})),
          ?assertEqual({error,"blah is not registered"},chatterl_store:archive_msg(Nick2,{erlang:localtime(),"blah","hey"})),
          ?assertEqual({ok,"Sent message to noobie"},chatterl_store:archive_msg(Nick2,{CreatedOn1,Nick1,"hey"})),
          ?assertEqual([],chatterl_store:get_messages(Nick2))
      end}},
     {"Client has access to archived messages",
      fun() ->
          ?assertEqual([],gen_server:call({global,Nick2},poll_messages)),
          ?assertEqual({ok,no_messages},chatterl_client:get_messages(Nick2))
      end},
     {timeout,50000,{"Client can send a private message to an logged out registered user.",
      fun() ->
          ?assertEqual({ok,"Sent message to noobie"},chatterl_client:private_msg(Nick2,Nick1,"sup"))
      end}},
     {"Client can retrieve archived messages once they log on",
      fun() ->
          chatterl_serv:login(Nick1,Password1),
          ?assertEqual([{CreatedOn1,{client,Nick2},"hey"},{CreatedOn1,{client,Nick2},"sup"}],gen_server:call({global,Nick1},poll_messages)),
          ?assertEqual([],chatterl_store:get_messages(Nick1))
      end}]}].
