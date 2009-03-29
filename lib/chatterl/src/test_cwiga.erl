-module(test_cwiga).

-include_lib("eunit/include/eunit.hrl").
-include_lib("chatterl.hrl").

-import(test_helpers,[check_response/2,check_json/1,http_request/3,http_login/2,set_params/1]).

-define(URL,"http://127.0.0.1:9000").

handles_test_() ->
  {Client,Group} = {"baph","nu"},
  [{setup,
    fun() ->
        inets:start(),
        chatterl:start(),
        chatterl_serv:create(Group,"nu room")
    end,
    fun(_) ->
        chatterl:stop()
    end,
    [{"CWIGA response to unknown response with a 404",
      fun() ->
          Response = http:request("http://127.0.0.1:9000/"),
          ?assertEqual(404, check_response(code,Response)),
          ?assertEqual("Object Not Found", check_response(status,Response)),
          ?assertEqual({"content-type","text/json"},
                       check_response(content_type,Response)),
          ?assertEqual(<<"Unknown command: /">>,
                       check_json(mochijson2:decode(check_response(body,Response))))
      end},
     {"CWIGA can retrieve an empty list of users",
      fun() ->
         Response = http:request("http://127.0.0.1:9000/users/list"),
          ?assertEqual(200,check_response(code,Response)),
          ?assertEqual({struct,[{<<"clients">>,[]}]},check_json(mochijson2:decode(check_response(body,Response))))
     end},
     {"CWIGA can list of users in a groups",
      fun() ->
          Response =  http:request("http://127.0.0.1:9000/users/list/" ++ Group),
          Response2 =  http:request("http://127.0.0.1:9000/users/list/" ++ "blah"),
          ?assertEqual(200,check_response(code,Response)),
          ?assertEqual({struct,[{<<"clients">>,[]}]},check_json(mochijson2:decode(check_response(body,Response)))),
          ?assertEqual(<<"Group: blah doesn't exist">>,check_json(mochijson2:decode(check_response(body,Response2))))
      end},
     {"CWIGA can retrieve a groups information",
      fun() ->
          Response =  http:request("http://127.0.0.1:9000/groups/info/" ++ Group),
          Response2 =  http:request("http://127.0.0.1:9000/groups/info/" ++ "blah"),
          ?assertEqual(200,check_response(code,Response)),
          ?assert(is_tuple(check_json(mochijson2:decode(check_response(body,Response))))),
          ?assertEqual(<<"Group doesn't exist!">>,check_json(mochijson2:decode(check_response(body,Response2))))
      end},
     {"CWIGA can retrieve a groups empty messages",
      fun() ->
          Response =  http:request("http://127.0.0.1:9000/groups/poll/" ++ Group),
          Response2 =  http:request("http://127.0.0.1:9000/groups/poll/" ++ "blah"),
          ?assertEqual(200,check_response(code,Response)),
          ?assertEqual(<<"Group: blah doesn't exist!">>,check_json(mochijson2:decode(check_response(body,Response2))))
      end},
     {"CWIGA can retrieve responses in XML format",
      fun() ->
         Response = http:request("http://127.0.0.1:9000/users/list.xml"),
         ?assertEqual({"content-type","text/xml"},check_response(content_type,Response))
      end},
     {"CWIGA can connect clients to chatterl",
      fun() ->
          Response = http:request("http://127.0.0.1:9000/users/connect/" ++ Client),
          Response2 = http:request("http://127.0.0.1:9000/users/connect/" ++ Client),
          ?assertEqual(200,check_response(code,Response)),
          ?assertEqual(<<"baph now connected">>,check_json(mochijson2:decode(check_response(body,Response)))),
          ?assertEqual(<<"baph is already connected">>,check_json(mochijson2:decode(check_response(body,Response2))))
      end},
     {"CWIGA can disconnect clients from chatterl",
      fun() ->
          Response = http:request("http://127.0.0.1:9000/users/disconnect/" ++ Client),
          Response2 = http:request("http://127.0.0.1:9000/users/disconnect/" ++ Client),
          ?assertEqual(200,check_response(code,Response)),
          ?assertEqual(<<"Disconnected">>,check_json(mochijson2:decode(check_response(body,Response)))),
          ?assertEqual(<<"Not connected">>,check_json(mochijson2:decode(check_response(body,Response2))))
      end}]}].

groups_handle_test_() ->
  {Client,Group} = {"baph","nu"},
  [{setup,
    fun() ->
        inets:start(),
        chatterl:start(),
        http:request("http://127.0.0.1:9000/users/connect/" ++ Client)
    end,
    fun(_) ->
        chatterl:stop()
    end,
    [{"CWIGA allows clients to retrieves a list of the groups they are connected to",
      fun() ->
          Response = http:request("http://127.0.0.1:9000/users/groups/" ++ Client),
          Response2 = http:request("http://127.0.0.1:9000/users/groups/" ++ "blah"),
          ?assertEqual(200,check_response(code,Response)),
          ?assertEqual(501,check_response(code,Response2)),
          ?assertEqual({struct,[{<<"groups">>,[]}]},check_json(mochijson2:decode(check_response(body,Response)))),
          ?assertEqual(<<"Client: blah doesn't exist">>,check_json(mochijson2:decode(check_response(body,Response2))))
      end},
     {"CWIGA allows clients to poll chatterl for messages",
      fun() ->
          Response = http:request("http://127.0.0.1:9000/users/poll/" ++ "blah"),
          Response2 = http:request("http://127.0.0.1:9000/users/poll/" ++ Client),
          ?assertEqual(501,check_response(code,Response)),
          ?assertEqual(200,check_response(code,Response2)),
          ?assertEqual(<<"Client: blah doesn't exist">>,check_json(mochijson2:decode(check_response(body,Response)))),
          ?assertEqual({struct,[{<<"messages">>,[]}]},check_json(mochijson2:decode(check_response(body,Response2))))
      end},
     {"CWIGA can list the groups on chatterl",
      fun() ->
          Response = http:request("http://127.0.0.1:9000/groups/list"),
          chatterl_serv:create(Group,"nu room"),
          Response2 = http:request("http://127.0.0.1:9000/groups/list"),
          ?assertEqual(200,check_response(code,Response)),
          ?assertEqual(200,check_response(code,Response2)),
          ?assertEqual({struct,[{<<"groups">>,[]}]},check_json(mochijson2:decode(check_response(body,Response)))),
          ?assert(Response /= Response2)
      end}]}].

groups_send_message_handle_test_() ->
  {Client,Client2,Group} = {"baph","baphled","nu"},
  [{setup,
    fun() ->
        inets:start(),
        chatterl:start(),
        chatterl_serv:create(Group,"nu room"),
        http:request("http://127.0.0.1:9000/users/connect/" ++ Client),
        http:request("http://127.0.0.1:9000/users/connect/" ++ Client2)
    end,
    fun(_) ->
        chatterl:stop()
    end,
    [{"CWIGA allows a client to join a group",
      fun() ->
          Args = [{"client",Client}],
          Body = set_params(Args),
          Response = http_request(post,?URL ++ "/groups/join/" ++ Group, Body),
          Response2 = http_request(post,?URL ++ "/groups/join/" ++ Group, Body),
          ?assertEqual(200,check_response(code,Response)),
          ?assertEqual(501,check_response(code,Response2)),
          ?assertEqual(<<"baph joined group">>,check_json(mochijson2:decode(check_response(body,Response)))),
          ?assertEqual(<<"Unable to connect!">>,check_json(mochijson2:decode(check_response(body,Response2))))
      end},
     {"CWIGA allows clients are unable to send messages a group if they are not connected to it",
      fun() ->
          Args = [{"msg","hey"},{"client",Client2}],
          Body = set_params(Args),
          Response = http_request(post,?URL ++ "/groups/send/" ++ Group, Body),
          ?assertEqual(404,check_response(code,Response)),
          ?assertEqual(<<"Must join group first!">>,check_json(mochijson2:decode(check_response(body,Response))))
      end},
     {"CWIGA allows clients to send messages to chatterl groups",
      fun() ->
          Args = [{"msg","hey"},{"client",Client}],
          Body = set_params(Args),
          Response = http_request(post,?URL ++ "/groups/send/" ++ Group, Body),
          ?assertEqual(200,check_response(code,Response)),
          ?assertEqual(<<"Message sent">>,check_json(mochijson2:decode(check_response(body,Response))))
      end},
     {"CWIGA allows clients are able to leave a chatterl group",
      fun() ->
          Args = [{"client",Client}],
          Body = set_params(Args),
          Response = http_request(post,?URL ++ "/groups/leave/" ++ Group, Body),
          Response2 = http_request(post,?URL ++ "/groups/leave/" ++ Group, Body),
          ?assertEqual(200,check_response(code,Response)),
          ?assertEqual(<<"baph has disconnected from nu">>,check_json(mochijson2:decode(check_response(body,Response)))),
          ?assertEqual(501,check_response(code,Response2))
      end}]}].

cwiga_registeration_based_test_() ->
  {Nick1,Name1,Email1,Password1} = {"noobie","noobie 1","noobie@noobie.com","blahblah"},
  [{setup,
    fun() ->
        inets:start(),
        chatterl:start()
    end,
    fun(_) ->
        chatterl:stop(),
        mnesia:clear_table(registered_user)
    end,
    [{"CWIGA does not allow clients to register if their passwords don't match",
      fun() ->
          Args = [{"pass2",Password1},{"pass1","adasd"},{"email",Email1},{"name",Name1}],
          Body = set_params(Args),
          Response = http_request(post,?URL ++ "/register/" ++ Nick1, Body),
          ?assertEqual(404,check_response(code,Response)),
          ?assertEqual(<<"noobie's passwords must match">>,check_json(mochijson2:decode(check_response(body,Response))))
      end},
     {"CWIGA allows client to register if they have the correct credentials",
      fun() ->
          Args = [{"pass2",Password1},{"pass1",Password1},{"email",Email1},{"name",Name1}],
          Body = set_params(Args),
          Response = http_request(post,?URL ++ "/register/" ++ Nick1, Body),
          ?assertEqual(200,check_response(code,Response)),
          ?assertEqual(<<"noobie is registered">>,check_json(mochijson2:decode(check_response(body,Response))))
      end},
    {"CWIGA does not allows clients to register if a nick is already in use",
      fun() ->
          Args = [{"pass2",Password1},{"pass1",Password1},{"email",Email1},{"name",Name1}],
          Body = set_params(Args),
          Response = http_request(post,?URL ++ "/register/" ++ Nick1, Body),
          ?assertEqual(404,check_response(code,Response)),
          ?assertEqual(<<"noobie is already registered">>,check_json(mochijson2:decode(check_response(body,Response))))
      end},
     {"CWIGA will not login clients if their credentials are not correct",
      fun() ->
          Args = [{"pass","blah"},{"login",Nick1}],
          Body = set_params(Args),
          Response = http_request(post,?URL ++ "/users/login", Body),
          ?assertEqual(501,check_response(code,Response)),
          ?assertEqual(<<"Unable to login">>,check_json(mochijson2:decode(check_response(body,Response))))
      end},
     {"CWIGA allows a registered client to login to chatterl",
      fun() ->
          Args = [{"pass",Password1},{"login",Nick1}],
          Body = set_params(Args),
          Response = http_request(post,?URL ++ "/users/login", Body),
          Response2 = http_request(post,?URL ++ "/users/login", Body),
          ?assertEqual(200,check_response(code,Response)),
          ?assertEqual(<<"noobie is logged in.">>,check_json(mochijson2:decode(check_response(body,Response)))),
          ?assertEqual(501,check_response(code,Response2)),
          ?assertEqual(<<"Already logged in">>,check_json(mochijson2:decode(check_response(body,Response2))))
      end},
     {"CWIGA alerts clients to the fact they need to be registered to login",
      fun() ->
          Args = [{"pass","blah"},{"login","blah"}],
          Body = set_params(Args),
          Response = http_request(post,?URL ++ "/users/login", Body),
          ?assertEqual(501,check_response(code,Response)),
          ?assertEqual(<<"Not registered">>,check_json(mochijson2:decode(check_response(body,Response))))
      end},
      {"CWIGA will not logout a client that is not already connected",
       fun() ->
           Args = [{"client","blah"}],
           Body = set_params(Args),
           Response = http_request(post,?URL ++ "/users/logout", Body),
           ?assertEqual(501,check_response(code,Response)),
           ?assertEqual(<<"Not logged in">>,check_json(mochijson2:decode(check_response(body,Response))))
      end},
      {"CWIGA will logout a client that is connected to chatterl",
       fun() ->
           Args = [{"client",Nick1}],
           Body = set_params(Args),
           Response = http_request(post,?URL ++ "/users/logout", Body),
           ?assertEqual(200,check_response(code,Response)),
           ?assertEqual(<<"noobie is logged out.">>,check_json(mochijson2:decode(check_response(body,Response))))
      end}]}].

cwiga_registeration_clients_can_get_archived_messages_test_() ->
  {Nick1,Name1,Email1,Password1} = {"noobie","noobie 1","noobie@noobie.com","blahblah"},
  Sender = "baft",
  [{setup,
    fun() ->
        inets:start(),
        chatterl:start(),
        Args = [{"pass2",Password1},{"pass1","adasd"},{"email",Email1},{"name",Name1}],
        Body = set_params(Args),
        http_request(post,?URL ++ "/register/" ++ Nick1, Body),
        http:request(?URL ++ "/users/connect/" ++ Sender)
    end,
    fun(_) ->
        chatterl:stop(),
        mnesia:clear_table(registered_user)
    end,
    [{"CWIGA does not allow clients to messages to clients that are not logged in or registered",
      fun() ->
          Args = [{"msg","hey"},{"client",Sender}],
          Body = set_params(Args),
          Response = http_request(post,?URL ++ "/users/send/" ++ Nick1, Body),
          ?assertEqual(501,check_response(code,Response)),
          ?assertEqual(<<"noobie is not connected!">>,check_json(mochijson2:decode(check_response(body,Response))))
      end},
    {"CWIGA does not allow clietns to send message if the sender does not exist",
      fun() ->
          Args = [{"msg","hey"},{"client",Sender}],
          Body = set_params(Args),
          Response = http_request(post,?URL ++ "/users/send/" ++ "blah", Body),
          ?assertEqual(501,check_response(code,Response)),
          ?assertEqual(<<"blah is not connected!">>,check_json(mochijson2:decode(check_response(body,Response))))
      end},
    {"CWIGA allows connected clients to send messages to registered clients",
      fun() ->
          Args = [{"msg","hey"},{"client",Nick1}],
          Body = set_params(Args),
          Response = http_request(post,?URL ++ "/users/send/" ++ Sender, Body),
          ?assertEqual(200,check_response(code,Response)),
          ?assertEqual(<<"Sending message to noobie...">>,check_json(mochijson2:decode(check_response(body,Response))))
      end}]}].
