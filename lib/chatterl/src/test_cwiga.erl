-module(test_cwiga).

-include_lib("eunit/include/eunit.hrl").
-include_lib("chatterl.hrl").

-import(test_helpers,[check_response/2,check_json/1]).

handles_test_() ->
  {Client,Group,ContentType} = {"baph","nu",["text/json"]},
  [{setup,
    fun() ->
        inets:start(),
        chatterl_serv:start(),
        chatterl_mid_man:start(),
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
     {"CWIGA can retrieve responses in XML format",
      fun() ->
         Response = http:request("http://127.0.0.1:8080/users/list.xml"),
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
