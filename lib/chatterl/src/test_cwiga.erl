-module(test_cwiga).

-include_lib("eunit/include/eunit.hrl").
-include_lib("chatterl.hrl").

-import(test_helpers,[check_response/2,check_json/1,http_request/3,set_params/1]).

-define(URL,"http://127.0.0.1:8080").

handles_test_() ->
  {Client,Group} = {"baph","nu"},
  [{setup,
    fun() ->
        inets:start(),
        chatterl_serv:start(),
        chatterl_mid_man:start(),
        chatterl_serv:create(Group,"nu room"),
        cwiga:start_link(8080)
    end,
    fun(_) ->
        chatterl_mid_man:stop(),
        chatterl_serv:stop(),
        cwiga:stop()
    end,
    [{"CWIGA response to unknown response with a 404",
      fun() ->
          Response = http:request("http://127.0.0.1:8080/"),
          ?assertEqual(404, check_response(code,Response)),
          ?assertEqual("Object Not Found", check_response(status,Response)),
          ?assertEqual({"content-type","text/json"},
                       check_response(content_type,Response)),
          ?assertEqual(<<"Unknown command: /">>,
                       check_json(mochijson2:decode(check_response(body,Response))))
      end},
     {"CWIGA can retrieve an empty list of users",
      fun() ->
         Response = http:request("http://127.0.0.1:8080/users/list"),
          ?assertEqual(200,check_response(code,Response)),
          ?assertEqual({struct,[{<<"clients">>,[]}]},check_json(mochijson2:decode(check_response(body,Response))))
     end},
     {"CWIGA can list of users in a groups",
      fun() ->
          Response =  http:request("http://127.0.0.1:8080/users/list/" ++ Group),
          Response2 =  http:request("http://127.0.0.1:8080/users/list/" ++ "blah"),
          ?assertEqual(200,check_response(code,Response)),
          ?assertEqual({struct,[{<<"clients">>,[]}]},check_json(mochijson2:decode(check_response(body,Response)))),
          ?assertEqual(<<"Group: blah doesn't exist">>,check_json(mochijson2:decode(check_response(body,Response2))))
      end},
     {"CWIGA can retrieve a groups information",
      fun() ->
          Response =  http:request("http://127.0.0.1:8080/groups/info/" ++ Group),
          Response2 =  http:request("http://127.0.0.1:8080/groups/info/" ++ "blah"),
          ?assertEqual(200,check_response(code,Response)),
          ?assert(is_tuple(check_json(mochijson2:decode(check_response(body,Response))))),
          ?assertEqual(<<"Group doesn't exist!">>,check_json(mochijson2:decode(check_response(body,Response2))))
      end},
     {"CWIGA can retrieve a groups empty messages",
      fun() ->
          Response =  http:request("http://127.0.0.1:8080/groups/poll/" ++ Group),
          Response2 =  http:request("http://127.0.0.1:8080/groups/poll/" ++ "blah"),
          ?assertEqual(200,check_response(code,Response)),
          ?assertEqual(<<"Group: blah doesn't exist!">>,check_json(mochijson2:decode(check_response(body,Response2))))
      end},
     {"CWIGA can retrieve responses in XML format",
      fun() ->
         Response = http:request("http://127.0.0.1:8080/list.xml"),
         ?assertEqual({"content-type","text/xml"},check_response(content_type,Response))
      end},
     {"CWIGA can connect clients to chatterl",
      fun() ->
          Response = http:request("http://127.0.0.1:8080/users/connect/" ++ Client),
          Response2 = http:request("http://127.0.0.1:8080/users/connect/" ++ Client),
          ?assertEqual(200,check_response(code,Response)),
          ?assertEqual(<<"baph now connected">>,check_json(mochijson2:decode(check_response(body,Response)))),
          ?assertEqual(<<"baph is already connected">>,check_json(mochijson2:decode(check_response(body,Response2))))
      end},
     {"CWIGA can disconnect clients from chatterl",
      fun() ->
          Response = http:request("http://127.0.0.1:8080/users/disconnect/" ++ Client),
          Response2 = http:request("http://127.0.0.1:8080/users/disconnect/" ++ Client),
          ?assertEqual(200,check_response(code,Response)),
          ?assertEqual(<<"Disconnected">>,check_json(mochijson2:decode(check_response(body,Response)))),
          ?assertEqual(<<"Not connected">>,check_json(mochijson2:decode(check_response(body,Response2))))
      end}]}].

groups_handle_test_() ->
  {Client,Group} = {"baph","nu"},
  [{setup,
    fun() ->
        inets:start(),
        chatterl_serv:start(),
        chatterl_mid_man:start(),
        cwiga:start_link(8080),
        http:request("http://127.0.0.1:8080/users/connect/" ++ Client)
    end,
    fun(_) ->
        chatterl_mid_man:stop(),
        chatterl_serv:stop(),
        cwiga:stop()
    end,
    [{"CWIGA allows clients to retrieves a list of the groups they are connected to",
      fun() ->
          Response = http:request("http://127.0.0.1:8080/users/groups/" ++ Client),
          Response2 = http:request("http://127.0.0.1:8080/users/groups/" ++ "blah"),
          ?assertEqual(200,check_response(code,Response)),
          ?assertEqual(501,check_response(code,Response2)),
          ?assertEqual({struct,[{<<"groups">>,[]}]},check_json(mochijson2:decode(check_response(body,Response)))),
          ?assertEqual(<<"Client: blah doesn't exist">>,check_json(mochijson2:decode(check_response(body,Response2))))
      end},
     {"CWIGA allows clients to poll chatterl for messages",
      fun() ->
          Response = http:request("http://127.0.0.1:8080/users/poll/" ++ "blah"),
          Response2 = http:request("http://127.0.0.1:8080/users/poll/" ++ Client),
          ?assertEqual(501,check_response(code,Response)),
          ?assertEqual(200,check_response(code,Response2)),
          ?assertEqual(<<"Client: blah doesn't exist">>,check_json(mochijson2:decode(check_response(body,Response)))),
          ?assertEqual({struct,[{<<"messages">>,[]}]},check_json(mochijson2:decode(check_response(body,Response2))))
      end},
     {"CWIGA can list the groups on chatterl",
      fun() ->
          Response = http:request("http://127.0.0.1:8080/groups/list"),
          chatterl_serv:create(Group,"nu room"),
          Response2 = http:request("http://127.0.0.1:8080/groups/list"),
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
        chatterl_serv:start(),
        chatterl_mid_man:start(),
        cwiga:start_link(8080),
        chatterl_serv:create(Group,"nu room"),
        http:request("http://127.0.0.1:8080/users/connect/" ++ Client),
        http:request("http://127.0.0.1:8080/users/connect/" ++ Client2)
    end,
    fun(_) ->
        chatterl_mid_man:stop(),
        chatterl_serv:stop(),
        cwiga:stop()
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
