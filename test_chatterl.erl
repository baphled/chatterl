-module(test_chatterl).

-include_lib("eunit/include/eunit.hrl").

chatterl_client_start_test_() ->
    [?_assertEqual({ok,connected},chatterl_client:start())].

