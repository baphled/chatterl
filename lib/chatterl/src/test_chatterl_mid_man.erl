-module(test_chatterl_mid_man).

-include_lib("eunit/include/eunit.hrl").
-include_lib("chatterl.hrl").

-import(test_helpers,[check_json/1]).

%% Test all our middle man json response
chatterl_mid_man_basics_test_() ->
  [{setup,
    fun() ->
        chatterl:start()
    end,
    fun(_) ->
        chatterl:stop()
    end,
    [{"Basic CWIGA tests.",
      fun() ->
          ?assertEqual(<<"Illegal content type!">>,
                       check_json(mochijson2:decode(chatterl_mid_man:user_list("text/json")))),
          ?assertEqual({struct,[{<<"clients">>,[]}]},
                       check_json(mochijson2:decode(chatterl_mid_man:user_list(["text/json"]))))
      end}]}].

chatterl_group_messages_test_() ->
  {Client,Group,ContentType} = {"baph","nu",["text/json"]},
  [{setup, fun() ->
               chatterl:start(),
               chatterl_mid_man:connect(ContentType,Client),
               chatterl_serv:create(Group,"nu room"),
               chatterl_mid_man:group_join(ContentType,{Group,Client})
           end,
    fun(_) -> chatterl:stop() end,
    [{timeout, 5000,
      fun() ->
          Result = {struct,[{<<"messages">>,[]}]},
          ?assertEqual(Result,
                       check_json(mochijson2:decode(chatterl_mid_man:group_poll(["text/json"],Group)))),
          ?assertEqual(<<"Must join group first!">>,
                       check_json(mochijson2:decode(chatterl_mid_man:group_send(["text/json"],{Group,"blah","hey"})))),
          ?assertEqual(<<"Group does not exist">>,
                       check_json(mochijson2:decode(chatterl_mid_man:group_send(["text/json"],{"blah",Client,"hey"})))),
          ?assertEqual(<<"Message sent">>,
                       check_json(mochijson2:decode(chatterl_mid_man:group_send(["text/json"],{Group,Client,"hey"})))),
          ?assert(Result /=  check_json(mochijson2:decode(chatterl_mid_man:group_poll(["text/json"],Group)))),
          ?assertEqual(<<"Group: blah doesn't exist!">>,
                       check_json(mochijson2:decode(chatterl_mid_man:group_poll(["text/json"],"blah"))))
      end}]}].

chatterl_mid_man_message_poll_test_() ->
  {Client1,Client2} = {"baft","boodah"},
  [{setup,
    fun() ->
        chatterl:start(),
        chatterl_serv:create("sum other room","nu room"),
        chatterl_mid_man:connect(["text/json"],Client1),
        chatterl_mid_man:connect(["text/json"],Client2)
    end,
    fun(_) ->
        chatterl:stop()
    end,
    [{"Chatterl Middle Man can retrieve messages",
      fun() ->
          ?assertEqual({struct,[{<<"messages">>,[]}]},
                       check_json(mochijson2:decode(chatterl_mid_man:user_poll(["text/json"],Client1)))),
          ?assertEqual(<<"Sending message to boodah...">>,
                       check_json(mochijson2:decode(chatterl_mid_man:user_msg(["text/json"],{Client1,Client2,"hey"})))),
          ?assertEqual(<<"Client: foobar doesn't exist">>,
                       check_json(mochijson2:decode(chatterl_mid_man:user_poll(["text/json"],"foobar")))),
          ?assertEqual({struct,[{<<"messages">>,[]}]},
                       check_json(mochijson2:decode(chatterl_mid_man:user_poll(["text/json"],Client1)))),
          ?assert({struct,[{<<"messages">>,[]}]} /= mochijson2:decode(chatterl_mid_man:user_poll(["text/json"],Client2)))
      end}]}].

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

chatterl_mid_man_registered_client_test_() ->
  {Nick1,Name1,Email1,Password1,Password2} = {"noobie","noobie 1","noobie@noobie.com","blahblah","asfdasdf"},
  [{setup,
    fun() ->
        chatterl:start()
    end,
    fun(_) ->
        chatterl:stop(),
        mnesia:delete_table(registered_user)
    end,
    [{timeout,5000,
      {"Client can register via chatterl_mid_man",
       fun() ->
           ?assertEqual(<<"noobie's passwords must match">>,
                        check_json(mochijson2:decode(chatterl_mid_man:register(["text/json"],{Nick1,{Name1,Email1,Password1,Password2}})))),
           ?assertEqual(<<"noobie is registered">>,
                        check_json(mochijson2:decode(chatterl_mid_man:register(["text/json"],{Nick1,{Name1,Email1,Password1,Password1}})))),
           ?assertEqual(<<"noobie is already registered">>,
                        check_json(mochijson2:decode(chatterl_mid_man:register(["text/json"],{Nick1,{Name1,Email1,Password1,Password1}}))))
       end}},
     {timeout,5000,
      {"Client can login via chatterl_mid_man",
       fun() ->
           ?assertEqual(<<"Unable to login">>,
                        check_json(mochijson2:decode(chatterl_mid_man:login(["text/json"],{Nick1,Password2})))),
           ?assertEqual(<<"noobie is logged in.">>,
                        check_json(mochijson2:decode(chatterl_mid_man:login(["text/json"],{Nick1,Password1})))),
           ?assertEqual(<<"Already logged in">>,
                        check_json(mochijson2:decode(chatterl_mid_man:login(["text/json"],{Nick1,Password1}))))
       end}},
     {"Client can logout via chatterl_mid_man",
      fun() ->
          ?assertEqual(<<"Not logged in">>,check_json(mochijson2:decode(chatterl_mid_man:logout(["text/json"],"blah")))),
          ?assertEqual(<<"noobie is logged out.">>,check_json(mochijson2:decode(chatterl_mid_man:logout(["text/json"],Nick1))))
      end}]}].

chatterl_mid_man_user_connect_test_() ->
  {Client1,Client2,Client3,Group} = {"baft","boodah","baphled","sum room"},
  [{setup,
    fun() ->
        chatterl:start(),
        chatterl_mid_man:connect(["text/json"],Client2),
        chatterl_serv:create(Group,"nu room")
    end,
    fun(_) ->
        chatterl:stop()
    end,
    [{"CWIGA retrieves client",
      fun() ->
          ?assertEqual({struct,[{<<"clients">>,[{struct,[{<<"client">>,<<"boodah">>}]}]}]},
                       check_json(mochijson2:decode(chatterl_mid_man:user_list(["text/json"]))))
      end},
     {"Clients can connect via CWIGA",
      fun() ->
          ?assertEqual(<<"baphled now connected">>,
                       check_json(mochijson2:decode(chatterl_mid_man:connect(["text/json"],Client3)))),
          ?assertEqual(<<"baphled is already connected">>,
                       check_json(mochijson2:decode(chatterl_mid_man:connect(["text/json"],Client3))))
      end},
     {"Clients can disconnect via CWIGA",
      fun() ->
          ?assertEqual(<<"Not connected">>,
                       check_json(mochijson2:decode(chatterl_mid_man:disconnect(["text/json"],Client1)))),
          ?assertEqual(<<"Disconnected">>,
                       check_json(mochijson2:decode(chatterl_mid_man:disconnect(["text/json"],Client3))))
      end},
     {"CWIGA can list clients",
      fun() ->
          ?assertEqual({struct,[{<<"clients">>,[]}]},
                       check_json(mochijson2:decode(chatterl_mid_man:user_list(["text/json"],Group)))),
          ?assertEqual(<<"Group: nonexistent doesn't exist">>,
                       check_json(mochijson2:decode(chatterl_mid_man:user_list(["text/json"],"nonexistent"))))
      end},
     {"CWIGA doesn't send client messages is not connected",
      fun() ->
          ?assertEqual(<<"blah is not connected!">>,
                       check_json(mochijson2:decode(chatterl_mid_man:user_msg(["text/json"],{"blah",Client2,"hey"})))),
          ?assertEqual(<<"baft is not connected!">>,
                       check_json(mochijson2:decode(chatterl_mid_man:user_msg(["text/json"],{Client1,"blah","hey"}))))
      end},
     {"CWIGA can send messages to connected clients",
      fun() ->
          ?assertEqual(<<"baft now connected">>,
                       check_json(mochijson2:decode(chatterl_mid_man:connect(["text/json"],Client1)))),
          ?assertEqual(<<"Sending message to boodah...">>,
                       check_json(mochijson2:decode(chatterl_mid_man:user_msg(["text/json"],{Client1,Client2,"hey"})))),
          ?assertEqual({struct,[{<<"messages">>,[]}]},
                       check_json(mochijson2:decode(chatterl_mid_man:user_poll(["text/json"],Client1))))
      end}]}].

chatterl_private_messages_test_() ->
  {Client1,Client2,Group} = {"baft","boodah","anuva"},
  [{setup,
    fun() ->
        chatterl:start(),
        chatterl_mid_man:connect(["text/json"],Client1),
        chatterl_mid_man:connect(["text/json"],Client2),
        chatterl_serv:create(Group,"nu room")
    end,
    fun(_) ->
        chatterl_mid_man:disconnect(["text/json"],Client1),
        chatterl_mid_man:disconnect(["text/json"],Client2),
        chatterl_serv:drop(Group),
        chatterl:stop()
    end,
    [{"CWIGA can send private messages & poll them",
      fun() ->
          Result = check_json(mochijson2:decode(chatterl_mid_man:user_poll(["text/json"],Client2))),
          ?assertEqual({struct,[{<<"messages">>,[]}]},Result),
          chatterl_mid_man:user_msg(["text/json"],{Client1,Client2,"hey u"}),
          ?assertEqual({struct,[{<<"messages">>,[]}]},
                       check_json(mochijson2:decode(chatterl_mid_man:user_poll(["text/json"],Client2)))),
          ?assert(Result /= check_json(mochijson2:decode(chatterl_mid_man:user_poll(["text/json"],Client2))))
      end}]}].


group_test_() ->
  {Room,Description,ContentType} = {"nu","nu room",["text/json"]},
  [{setup, fun() ->
               chatterl:start()
           end,
    fun(_) -> chatterl:stop() end,
    [{timeout, 5000,
      {"Groups can be created",
       fun() ->
          ?assertEqual({struct,[{<<"groups">>,[]}]},check_json(mochijson2:decode(chatterl_mid_man:group_list(ContentType)))),
          ?assertEqual(<<"Group: nu added">>,
                       check_json(mochijson2:decode(chatterl_mid_man:group_create(ContentType,{Room,Description})))),
          ?assertEqual(<<"Unable to create group: nu">>,
                       check_json(mochijson2:decode(chatterl_mid_man:group_create(ContentType,{Room,Description}))))
       end}},
      {"Groups can group destroyed",
       fun() ->
          % abit lazy but not sure how to check the creation date dynamically atm.
          ?assert(erlang:is_tuple(check_json(mochijson2:decode(chatterl_mid_man:group_info(ContentType,Room))))),
          ?assertEqual(<<"Group dropped nu">>,
                       check_json(mochijson2:decode(chatterl_mid_man:group_drop(ContentType,Room)))),
          ?assertEqual(<<"Can not find nu">>,
                       check_json(mochijson2:decode(chatterl_mid_man:group_drop(ContentType,Room)))),
          ?assertEqual(<<"Group doesn't exist!">>,
                       check_json(mochijson2:decode(chatterl_mid_man:group_info(ContentType,Room))))
       end}]}].

groups_user_functionality_test_() ->
  {Client1,Client2,Group,ContentType} = {"baph","boodah","nu",["text/json"]},
  [{setup,
    fun() ->
        chatterl:start(),
        chatterl_mid_man:connect(ContentType,Client1),
        chatterl_mid_man:connect(ContentType,Client2),
        chatterl_serv:create(Group,"nu room"),
        chatterl_mid_man:group_join(ContentType,{Group,Client2})
    end,
    fun(_) ->
        chatterl:stop()
    end,
    [{"CWIGA allows clients to join groups",
      fun() ->
          ?assertEqual(<<"Group: blah doesn't exist">>,
                       check_json(mochijson2:decode(chatterl_mid_man:group_join(ContentType,{"blah",Client2})))),
          ?assertEqual({struct,[{<<"groups">>,[]}]},
                       check_json(mochijson2:decode(chatterl_mid_man:user_groups(ContentType,Client1)))),
          ?assertEqual(<<"Client: blah doesn't exist">>,
                       check_json(mochijson2:decode(chatterl_mid_man:user_groups(ContentType,"blah"))))
      end},
     {"CWIGA can list clients joined to groups",
      fun() ->
          ?assertEqual([Group],gen_server:call({global,Client2},groups)),
          ?assertEqual({struct,[{<<"groups">>,[{struct,[{<<"group">>,<<"nu">>}]}]}]},
                       check_json(mochijson2:decode(chatterl_mid_man:user_groups(ContentType,Client2)))),
          ?assertEqual({struct,[{<<"groups">>,[{struct,[{<<"group">>,<<"nu">>}]}]}]},
                       check_json(mochijson2:decode(chatterl_mid_man:group_list(ContentType))))
      end},
     {"CWIGA allows clients to leave groups",
      fun() ->
          ?assertEqual(<<"Group: blah doesn't exist">>,
                       check_json(mochijson2:decode(chatterl_mid_man:group_leave(ContentType,{"blah",Client2})))),
          ?assertEqual(<<"User not joined">>,
                       check_json(mochijson2:decode(chatterl_mid_man:group_leave(ContentType,{"blah","blah"})))),
          ?assertEqual(<<"boodah has disconnected from nu">>,
                       check_json(mochijson2:decode(chatterl_mid_man:group_leave(ContentType,{Group,Client2}))))
      end}]}].

registered_user_list_test_() ->
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
      {"Registerd clients are retrievable through chatterl_mid_man",
       fun() ->
          ?assertEqual({ok,"noobie is registered"},chatterl_store:register(Nick1,{Name1,Email1,Password1,Password1})),
          ?assertEqual([{Nick1,Name1,Email1}],chatterl_store:registered()),
          ?assertEqual({struct,[{<<"registered">>,
                                 {struct,[{<<"client">>,[{struct,[{<<"nick">>,<<"noobie">>}]},
                                                         {struct,[{<<"name">>,<<"noobie 1">>}]},
                                                         {struct,[{<<"email">>,<<"noobie@noobie.com">>}]}]}]}
                                }]},
                       check_json(mochijson2:decode(chatterl_mid_man:registered_list(ContentType))))
      end}},
     {"Multiple clients are listed as expected via chatterl_mid_man",
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
     end}]}].
