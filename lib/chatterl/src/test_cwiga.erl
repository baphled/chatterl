-module(test_cwiga).

-include_lib("eunit/include/eunit.hrl").
-include_lib("chatterl.hrl").

-import(test_helpers,[check_response/2,check_json/1]).
handles_test_() ->
  [{setup,
    fun() ->
        inets:start(),
        cwiga:start_link(8080)
    end,
    fun(_) ->
        cwiga:stop()
    end,
    [fun() ->
         Response = http:request("http://127.0.0.1:8080/"),
         ?assertEqual(404, check_response(code,Response)),
         ?assertEqual("Object Not Found", check_response(status,Response)),
         ?assertEqual({"content-type","text/json"}, check_response(content_type,Response)),
         ?assertEqual(<<"Unknown command: /">>, check_json(mochijson2:decode(check_response(body,Response))))
     end]}].
