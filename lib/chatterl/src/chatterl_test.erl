-module(chatterl_test).
-include_lib("eunit/include/eunit.hrl").

%% Test that our groups function as expected.
%% Instead of testing that the group can be created we'll create and group drops on setup.

chatterl_serv_stop_test_() ->
  [{setup, fun() ->
               {ok,Pid} = chatterl_serv:start(),
               register(chatterl_serv_tests, Pid),
               Pid end,
    fun(_) ->
        chatterl_serv:stop() end,
    [fun() ->
         {Client1,Client2,Group1,Group2,Description} = {"blah","noob","room","anuva","nu room"},
         ?assertEqual([],chatterl_serv:list_groups()),
         ?assertEqual([],chatterl_serv:list_users()),
         ?assertEqual({error,"Group doesn't exist!"},chatterl_serv:list_users(Group1)),
         ?assert(erlang:is_tuple(chatterl_serv:create(Group1,Description))),
         ?assertEqual([],chatterl_serv:list_users(Group1)),
         ?assertEqual(["room"],chatterl_serv:list_groups()),
         chatterl_serv:create(Group2,"anuva room"),
         ?assertEqual([Group2,"room"],chatterl_serv:list_groups()),
         chatterl_serv:create("room1","room1"),
         ?assertEqual([Group2,"room","room1"],chatterl_serv:list_groups()),
         ?assertEqual({ok,"connected"},chatterl_serv:connect(Client1)),
         ?assertEqual([Client1],chatterl_serv:list_users()),
         ?assertEqual({error,"Unable to connect."},chatterl_serv:connect(Client1)),
         ?assertEqual({error,"Unable to disconnect noone"},chatterl_serv:disconnect("noone")),
         ?assertEqual({ok,"User disconnected: blah"},chatterl_serv:disconnect(Client1)),
         ?assertEqual({error,"Can not find group."},chatterl_serv:group_description("anuva1")),
         ?assertEqual({description,Description},chatterl_serv:group_description(Group1)),
         ?assert(erlang:is_tuple(gen_server:call({global, chatterl_serv}, {get_group, Group1}))),
         ?assertEqual({error,"Can not find blah"},chatterl_serv:drop("blah")),
         ?assertEqual({ok,"Group dropped anuva"},chatterl_serv:drop(Group2)),
         ?assert(erlang:is_list(gen_server:call({global,chatterl_serv},{group_info,Group1})))
     end]}].

chatterl_group_info_test_() ->
  [{setup, fun() ->
               {Group,Description} = {"nu1","a nu room"},
               {ok, Pid} = start_group(Group,Description),
               register(chatterl_groups_tests, Pid),
               Pid end,
    fun(_) ->
        chatterl_serv:stop() end,
    [fun() ->
         Group = "nu1",
         ?assert(erlang:is_tuple(gen_server:call({global, chatterl_serv}, {get_group, Group}, infinity))),
         ?assertEqual({name,"nu1"},gen_server:call({global,Group},name)),
         ?assertEqual({description,"a nu room"}, gen_server:call({global,Group},description)),
         ?assert(erlang:is_tuple(gen_server:call({global,Group},created))),
         ?assertEqual([],gen_server:call({global,Group},poll_messages)),
         ?assertEqual(stopped,chatterl_groups:stop(Group))
     end]}].

chatterl_client_handle_test_() ->
  [{setup, fun() ->
               {Client1,Client2,Group,Description} = {"noobie","blah","anuva1","a nu room"},
               {ok, Pid} = start_client(Client1,Group,Description),
               chatterl_client:start(Client2),
               gen_server:call({global,Client1},{join_group,Group}),
               register(chatterl_client_handle_tests, Pid),
               Pid end,
    fun(_) ->
        chatterl_serv:stop() end,
   [fun() ->
        {Client1,Client2,Group} = {"noobie","blah","anuva1"},
        ?assertEqual({name,Client1}, gen_server:call({global,Client1},client_name)),
        ?assert(erlang:is_list(gen_server:call({global,Client1},groups))),
        ?assertEqual([], gen_server:call({global,Client2},groups)),
        ?assertEqual([], gen_server:call({global,Client2},poll_messages)),
        ?assertEqual({error,"Unknown error!"}, gen_server:call({global,Client2},{join_group,"none"})),
        ?assertEqual({error,"Unable to connect!"}, gen_server:call({global,Client1},{join_group,Group})),
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
    end ]}].

%% Test our groups functionality
chatterl_group_handle_test_() ->
  [{setup, fun() ->
               {Client,Group,Description} = {"baft","anuva","a nu room"},
               {ok, Pid} = start_client(Client,Group,Description),
               register(chatterl_client_join_tests, Pid),
               Pid end,
    fun(_) ->
        chatterl_serv:stop() end,
   [fun() ->
        {Client,Group} = {"baft","anuva"},
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
        ?assert(erlang:is_list(gen_server:call({global,Group},poll_messages))),
        ?assertEqual(stopped,chatterl_client:stop(Client))
        end]}].


%% Helper functions.
start_client(Client,Group,Description) ->
  {ok,Pid} = start_group(Group,Description),
  chatterl_client:start(Client),
  {ok,Pid}.

start_group(Group,Description) ->
  {ok,Pid} = chatterl_serv:start(),
  chatterl_serv:create(Group,Description),
  {ok,Pid}.
