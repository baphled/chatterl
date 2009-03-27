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