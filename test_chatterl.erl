-module(test_chatterl).

-include_lib("eunit/include/eunit.hrl").

chatterl_client_start_test_() ->
    [?_assertEqual({ok,connected},chatterl_client:start())].

chatterl_client_register_test_() ->
    [?_assertEqual({error, not_valid}, chatterl_client:login("bobby","fisher"))].

