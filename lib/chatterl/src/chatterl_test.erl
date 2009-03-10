-module(chatterl_test).
-include_lib("eunit/include/eunit.hrl").

%% Test chatterl_serv functionality
chatterl_serv_test_() ->
  [{setup, fun() ->
               {ok,Pid} = chatterl:start(),
               register(chatterl_serv_tests, Pid),
               Pid end,
    fun(_) ->
        chatterl:stop() end,
    [fun() ->
         {Client1,Client2,Group1,Group2,Description} = {"blah","noob","room","anuva","nu room"},
         ?assertEqual([],chatterl_serv:list_groups()),
         ?assertEqual([],chatterl_serv:list_users()),
         ?assertEqual({error,"Group doesn't exist!"},chatterl_serv:list_users(Group1)),
         ?assert(erlang:is_tuple(chatterl_serv:create(Group1,Description))),
         ?assertEqual({error,already_created},chatterl_serv:create(Group1,Description)),
         ?assertEqual([],chatterl_serv:list_users(Group1)),
         ?assertEqual(["room"],chatterl_serv:list_groups()),
         chatterl_serv:create(Group2,"anuva room"),
         ?assertEqual([Group2,"room"],chatterl_serv:list_groups()),
         chatterl_serv:create("room1","room1"),
         ?assertEqual([Group2,"room","room1"],chatterl_serv:list_groups()),
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

chatterl_client_handle_test_() ->
  {Client1,Client2,Group,Description} = {"noobie","blah","anuva1","a nu room"},
  [{setup, fun() ->
               start_client(Client1,Group,Description),
               chatterl_client:start(Client2) end,
    fun(_) ->
        chatterl_serv:disconnect(Client1),
        chatterl_serv:disconnect(Client2),
        chatterl:stop() end,
    [{timeout, 1000,
      fun() ->
          ?assertEqual({name,Client1}, gen_server:call({global,Client1},client_name)),
          ?assert(erlang:is_list(gen_server:call({global,Client1},groups))),
          ?assertEqual([], gen_server:call({global,Client2},groups)),
          ?assertEqual([], gen_server:call({global,Client2},poll_messages)),
          ?assertEqual({error,"Unknown error!"}, gen_server:call({global,Client2},{join_group,"none"})),
          ?assertEqual({ok,joined_group}, gen_server:call({global,Client1},{join_group,Group})),
          %?assertEqual({ok,joined_group}, gen_server:call({global,Client2},{join_group,Group})),
          ?assertEqual(["anuva1"],gen_server:call({global,Client1},groups)),
          ?assertEqual({error, user_not_joined}, gen_server:call({global,Client2},{group_msg,Group,"sup"})),
          ?assertEqual({ok,msg_sent}, gen_server:call({global,Client1},{private_msg,Client2,"sup"})),
          ?assertEqual({error,"Cannot find user!"}, gen_server:call({global,Client1},{private_msg,"noob","sup"})),
          ?assertEqual({ok,msg_sent}, gen_server:call({global,Client2},{private_msg,Client1,"sup"})),
          ?assertEqual({ok,msg_sent}, chatterl_client:private_msg(Client2,Client1,"sup")),
          ?assertEqual({error,"Can not send to self!"}, gen_server:call({global,Client1},{private_msg,Client1,"sup"})),
          ?assertEqual({error,"Not connected"}, gen_server:call({global,Client2},{leave_group,Group})),
          ?assertEqual({ok, drop_group}, gen_server:call({global,Client1},{leave_group,Group})),
          ?assert(erlang:is_list(gen_server:call({global,Client1},poll_messages))),
          ?assertEqual(stopped,gen_server:call({global,Client2},{stop,"because"}))
      end} ]}].

%% Test our groups functionality
chatterl_group_handle_test_() ->
  {Client,Group,Description} = {"boodah","anuva","a nu room"},
  [{setup, fun() ->
               start_client(Client,Group,Description) end,
    fun(_) ->
        chatterl_serv:disconnect(Client),
        chatterl:stop() end,
   [{timeout, 5000,
     fun() ->
        ?assert(erlang:is_tuple(gen_server:call({global, chatterl_serv}, {get_group, Group}, infinity))),
        ?assertEqual({name,Group},gen_server:call({global,Group},name)),
        ?assertEqual({description,"a nu room"}, gen_server:call({global,Group},description)),
        ?assert(erlang:is_tuple(gen_server:call({global,Group},created))),
        ?assertEqual([],gen_server:call({global,Group},list_users)),
        ?assertEqual([],gen_server:call({global,Group},poll_messages)),
        ?assertEqual({ok, "User added"},gen_server:call({global,Group},{join,Client})),
        ?assert(erlang:is_list(gen_server:call({global,Group},list_users))),
        ?assertEqual({error, "Already joined"},gen_server:call({global,Group},{join,Client})),
        ?assertEqual({error, "not connected"},gen_server:call({global,Group},{join,"nonUsers"})),
        ?assertEqual({error, "Invalid user name"},gen_server:call({global,Group},{join,{"nonUsers"}})),
        ?assertEqual({ok, dropped}, gen_server:call({global,Group},{leave,Client})),
        ?assertEqual({error, "Not connected"}, gen_server:call({global,Group},{leave,Client})),
        ?assertEqual([],gen_server:call({global,Group},list_users)),
        ?assertEqual({ok, "User added"},gen_server:call({global,Group},{join,Client})),
        ?assertMatch({error, user_not_joined},gen_server:call({global,Group},{send_msg,"blah","hey"})),
        ?assertEqual({ok, msg_sent},gen_server:call({global,Group},{send_msg,Client,"hey"})),
        ?assertEqual({error,already_sent},gen_server:call({global,Group},{send_msg,Client,"heya"})),
        ?assert(erlang:is_list(gen_server:call({global,Group},poll_messages)))
     end}]}].

chatterl_mid_man_message_json_test_() ->
  {Client1,Client2} = {"baft","boodah"},
  [{setup, fun() ->
               chatterl:start(),
               chatterl_serv:create("sum other room","nu room"),
               chatterl_mid_man:connect(["text/json"],Client1),
               chatterl_mid_man:connect(["text/json"],Client2)
           end,
    fun(_) ->
        chatterl_mid_man:disconnect(["text/json"],Client1),
        chatterl_mid_man:disconnect(["text/json"],Client2),
        chatterl:stop() end,
    [{timeout, 5000,
      fun() ->

          ?assertEqual({struct,[{<<"chatterl">>,{struct,[{<<"response">>,{struct,[{<<"success">>,{struct,[{<<"messages">>,[]}]}}]}}]}}]},
                       mochijson2:decode(chatterl_mid_man:user_poll(["text/json"],Client1))),
          ?assertEqual({struct,[{<<"chatterl">>,
                                 {struct,[{<<"response">>,
                                           {struct,[{<<"success">>,<<"Sending message to boodah...">>}]}}]}}]},
                       mochijson2:decode(chatterl_mid_man:user_msg(["text/json"],{Client1,Client2,"hey"}))),
          ?assertEqual({struct,[{<<"chatterl">>,
                                 {struct,[{<<"response">>,{struct,[{<<"failure">>,<<"Client: foobar doesn't exist">>}]}}]}}]},
                       mochijson2:decode(chatterl_mid_man:user_poll(["text/json"],"foobar"))),
          ?assertEqual({struct,[{<<"chatterl">>,{struct,
                                                 [{<<"response">>,{struct,[{<<"success">>,
                                                                            {struct,[{<<"messages">>,[]}]}}]}}]}}]},
                       mochijson2:decode(chatterl_mid_man:user_poll(["text/json"],Client1))),
          ?assert(erlang:is_tuple(mochijson2:decode(chatterl_mid_man:user_poll(["text/json"],Client2))))
      end}]}].

%% Test all our middle man json response
chatterl_mid_man_json_test_() ->
  {Client1,Client2,Client3,Group} = {"baft","boodah","baphled","sum room"},
  [{setup, fun() ->
               chatterl:start(),
               chatterl_mid_man:connect(["text/json"],Client2),
               chatterl_serv:create("sum room","nu room") end,
    fun(_) ->
        chatterl:stop() end,
    [{timeout, 5000,
      fun() ->
          {struct,[{<<"chatterl">>,{struct,[{<<"response">>,{struct,[{<<"success">>,{struct,[Results]}}]}}]}}]} =
             mochijson2:decode(chatterl_mid_man:user_list(["text/json"])),
          ?assertEqual([],chatterl_serv:list_users()),
          ?assertEqual({<<"clients">>,[]},Results),
          ?assertEqual({struct,[{<<"chatterl">>,{struct,[{<<"response">>,{struct,[{<<"success">>,{struct,[{<<"clients">>,[]}]}}]}}]}}]},
                       mochijson2:decode(chatterl_mid_man:user_list(["text/json"]))),
          ?assertEqual({struct,[{<<"chatterl">>,{struct,[{<<"response">>,{struct,[{<<"error">>,<<"Illegal content type!">>}]}}]}}]},
                       mochijson2:decode(chatterl_mid_man:user_list("text/json"))),
          {struct,[{<<"chatterl">>,{struct,[{<<"response">>,{struct,[Results1]}}]}}]} =
             mochijson2:decode(chatterl_mid_man:connect(["text/json"],Client3)),
          ?assertEqual({<<"success">>,<<"baphled now connected">>},Results1),
          {struct,[{<<"chatterl">>,{struct,[{<<"response">>,{struct,[{<<"failure">>,Result2}]}}]}}]} =
                       mochijson2:decode(chatterl_mid_man:connect(["text/json"],Client3)),
          ?assertEqual(<<"baphled is already connected">>,Result2),
          ?assertEqual({struct,
                      [{<<"chatterl">>,{struct,
                                        [{<<"response">>,{struct,
                                                          [{<<"success">>,{struct,
                                                                           [{<<"clients">>,
                                                                             [{struct,[{<<"client">>,<<"baphled">>}]}]}]}}]}}]}}]},
                     mochijson2:decode(chatterl_mid_man:user_list(["text/json"]))),
        ?assertEqual({struct,[{<<"chatterl">>,{struct,[{<<"response">>,{struct,[{<<"failure">>,<<"Not connected">>}]}}]}}]},
                     mochijson2:decode(chatterl_mid_man:disconnect(["text/json"],Client2))),
        ?assertEqual({struct,[{<<"chatterl">>,{struct,[{<<"response">>,{struct,[{<<"success">>,<<"Disconnected">>}]}}]}}]},
                     mochijson2:decode(chatterl_mid_man:disconnect(["text/json"],Client3))),
        ?assertEqual({struct,[{<<"chatterl">>,
          {struct,[{<<"response">>,{struct,[{<<"success">>,{struct,[{<<"clients">>,[]}]}}]}}]}}]},
                     mochijson2:decode(chatterl_mid_man:user_list(["text/json"],Group))),
        ?assertEqual({struct,[{<<"chatterl">>,
          {struct,[{<<"response">>,{struct,[{<<"error">>,<<"Group: nonexistent doesn't exist">>}]}}]}}]},
                     mochijson2:decode(chatterl_mid_man:user_list(["text/json"],"nonexistent"))),
        ?assertEqual({struct,[{<<"chatterl">>,
          {struct,[{<<"response">>,{struct,[{<<"failure">>,<<"Client doesn't exist!">>}]}}]}}]},
                     mochijson2:decode(chatterl_mid_man:user_msg(["text/json"],{"blah",Client2,"hey"}))),
      {struct,[{<<"chatterl">>,{struct,[{<<"response">>,{struct,[{<<"failure">>,Result3}]}}]}}]} =
                     mochijson2:decode(chatterl_mid_man:user_msg(["text/json"],{Client1,"blah","hey"})),
          ?assertEqual(<<"blah is not connected!">>,Result3)
    end}]}].


%% Helper functions.
start_client(Client,Group,Description) ->
  start_group(Group,Description),
  chatterl_client:start(Client).

start_group(Group,Description) ->
  chatterl:start(),
  chatterl_serv:create(Group,Description).
