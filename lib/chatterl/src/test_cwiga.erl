%%%----------------------------------------------------------------
%%% @author Yomi Colledge <yomi@boodah.net>
%%% @doc
%%% Test cases for CWIGA
%%%
%%% @end
%%% @copyright 2009 Yomi Colledge
%%%----------------------------------------------------------------
-module(test_cwiga).

-include_lib("eunit/include/eunit.hrl").
-include_lib("chatterl.hrl").

-import(test_helpers,[check_response/2,
                      check_json/1,
                      cwiga_request/2,
                      cwiga_register/1,
                      http_request/3,
                      http_login/2,
                      http_login/4,
                      set_params/1]).

-define(URL,"http://127.0.0.1:9000").

handles_test_() ->
  {Nick1,Name1,Email1,Password1} = {"noobie","noobie 1","noobie@noobie.com","blahblah"},
  {Client,Group} = {"baph","nu"},
  [{setup,
    fun() ->
        inets:start(),
        chatterl:start(),
        cwiga_register({Nick1,Name1,Email1,Password1}),
        chatterl_serv:create(Group,"nu room")
    end,
    fun(_) ->
        chatterl:stop()
    end,
    [{"CWIGA response to unknown response with a 404",
      fun() ->
          Response = http:request(?URL),
          ?assertEqual(404, check_response(code,Response)),
          ?assertEqual("Object Not Found", check_response(status,Response)),
          ?assertEqual({"content-type","text/json"},
                       check_response(content_type,Response)),
          ?assertEqual(<<"Unknown command: /">>,
                       check_json(check_response(body,Response)))
      end},
     {"CWIGA disallows client to retrieve a list of users if they are not authorised",
     fun() ->
         Response = http:request(?URL "/users/list/"),
         ?assertEqual(401,check_response(code,Response))
     end},
     {"CWIGA can retrieve an empty list of users",
      fun() ->
          Response = http_login(?URL "/users/list/",{Nick1,Password1}),
          ?assertEqual(200,check_response(code,Response)),
          ?assertEqual({struct,[{<<"clients">>,[]}]},check_json(check_response(body,Response)))
     end},
     {"CWIGA can list of users in a groups",
      fun() ->
          Response =  http_login(?URL "/users/list/" ++ Group,{Nick1,Password1}),
          Response2 =  http_login(?URL "/users/list/" ++ "blah",{Nick1,Password1}),
          ?assertEqual(200,check_response(code,Response)),
          ?assertEqual({struct,[{<<"clients">>,[]}]},check_json(check_response(body,Response))),
          ?assertEqual(<<"Group: blah doesn't exist">>,check_json(check_response(body,Response2)))
      end},
     {"CWIGA disallows retrieving group information if the client is not authorised",
      fun() ->
          Response =  http:request(?URL "/groups/info/" ++ Group),
          ?assertEqual(401,check_response(code,Response)),
          ?assertEqual(<<"Need to authorize">>,check_json(check_response(body,Response)))
      end},
     {"CWIGA can retrieve a groups information",
      fun() ->
          Response =  http_login(?URL "/groups/info/" ++ "blah",{Nick1,Password1}),
          Response2 =  http_login(?URL "/groups/info/" ++ Group,{Nick1,Password1}),
          ?assertEqual(200,check_response(code,Response2)),
          ?assertEqual(<<"Group doesn't exist!">>,check_json(check_response(body,Response))),
          ?assert(is_tuple(check_json(check_response(body,Response2))))
      end},
     {"CWIGA disallows clients from polling groups unless they are authorised",
      fun() ->
          Response =  http:request(?URL "/groups/poll/" ++ Group),
          ?assertEqual(401,check_response(code,Response))
      end},
      {"CWIGA will give an error if the group does not exist",
       fun() ->
           Response = http_login(?URL "/groups/poll/" ++ "blah",{Nick1,Password1}),
           ?assertEqual(404,check_response(code,Response)),
           ?assertEqual(<<"Group: blah doesn't exist!">>,check_json(check_response(body,Response)))
      end},
     {"CWIGA can retrieve a groups empty messages",
      fun() ->
          Response = http_login(?URL "/groups/poll/" ++ Group,{Nick1,Password1}),
          ?assertEqual(200,check_response(code,Response)),
          ?assertEqual({struct,[{<<"messages">>,[]}]},check_json(check_response(body,Response)))
      end},
     {"CWIGA can retrieve responses in XML format",
      fun() ->
         Response = http:request(?URL "/users/list.xml"),
         ?assertEqual({"content-type","text/xml"},check_response(content_type,Response))
      end},
     {"CWIGA can connect clients to chatterl",
      fun() ->
          Response = http:request(?URL "/users/connect/" ++ Client),
          Response2 = http:request(?URL "/users/connect/" ++ Client),
          ?assertEqual(200,check_response(code,Response)),
          ?assertEqual(<<"baph now connected">>,check_json(check_response(body,Response))),
          ?assertEqual(<<"baph is already connected">>,check_json(check_response(body,Response2)))
      end},
     {"CWIGA can disconnect clients from chatterl",
      fun() ->
          Response = http:request(?URL "/users/disconnect/" ++ Client),
          Response2 = http:request(?URL "/users/disconnect/" ++ Client),
          ?assertEqual(200,check_response(code,Response)),
          ?assertEqual(<<"Disconnected">>,check_json(check_response(body,Response))),
          ?assertEqual(<<"Not connected">>,check_json(check_response(body,Response2)))
      end}]}].

groups_handle_test_() ->
  {Nick1,Name1,Email1,Password1} = {"noobie","noobie 1","noobie@noobie.com","blahblah"},
  {Client,Group} = {"baph","nu"},
  [{setup,
    fun() ->
        inets:start(),
        chatterl:start(),
        cwiga_register({Nick1,Name1,Email1,Password1}),
        http:request(?URL "/users/connect/" ++ Client)
    end,
    fun(_) ->
        chatterl:stop()
    end,
    [{"CWIGA disallows unauthorised clients to list groups a user is connect to",
      fun() ->
          Response = http:request(?URL "/users/groups/" ++ Client),
          ?assertEqual(401,check_response(code,Response))
      end},
     {"CWIGA allows clients to retrieves a list of the groups they are connected to",
      fun() ->
          Response = http_login(?URL "/users/groups/" ++ Client,{Nick1,Password1}),
          Response2 = http_login(?URL "/users/groups/" ++ "blah",{Nick1,Password1}),
          ?assertEqual(200,check_response(code,Response)),
          ?assertEqual(500,check_response(code,Response2)),
          ?assertEqual({struct,[{<<"groups">>,[]}]},check_json(check_response(body,Response))),
          ?assertEqual(<<"Client: blah doesn't exist">>,check_json(check_response(body,Response2)))
      end},
     {"CWIGA disallows a client from polling for their messages",
      fun() ->
          Response = http:request(?URL "/users/poll/" ++ Client),
          ?assertEqual(401,check_response(code,Response))
          end},
     {"CWIGA allows clients to poll chatterl for messages",
      fun() ->
          Response = http_login(?URL "/users/poll/" ++ "blah",{Nick1,Password1}),
          Response2 = http_login(?URL "/users/poll/" ++ Client,{Nick1,Password1}),
          ?assertEqual(500,check_response(code,Response)),
          ?assertEqual(200,check_response(code,Response2)),
          ?assertEqual(<<"Client: blah doesn't exist">>,check_json(check_response(body,Response))),
          ?assertEqual({struct,[{<<"messages">>,[]}]},check_json(check_response(body,Response2)))
      end},
     {"CWIGA clients unable to retrieve groups list if not authorised",
      fun() ->
          Response = http:request(?URL "/groups/list"),
          ?assertEqual(401,check_response(code,Response))
          end},
     {"CWIGA can list the groups on chatterl",
      fun() ->
          Response = http_login(?URL "/groups/list",{Nick1,Password1}),
          chatterl_serv:create(Group,"nu room"),
          Response2 = http_login(?URL "/groups/list",{Nick1,Password1}),
          ?assertEqual(200,check_response(code,Response)),
          ?assertEqual(200,check_response(code,Response2)),
          ?assertEqual({struct,[{<<"groups">>,[]}]},check_json(check_response(body,Response))),
          ?assert(Response /= Response2)
      end}]}].

groups_send_message_handle_test_() ->
  {Nick1,Name1,Email1,Password1} = {"noobie","noobie 1","noobie@noobie.com","blahblah"},
  {Client,Client2,Group} = {"baph","baphled","nu"},
  [{setup,
    fun() ->
        inets:start(),
        chatterl:start(),
        cwiga_register({Nick1,Name1,Email1,Password1}),
        chatterl_serv:create(Group,"nu room"),
        http:request(?URL "/users/connect/" ++ Client),
        http:request(?URL "/users/connect/" ++ Client2)
    end,
    fun(_) ->
        chatterl:stop(),
        mnesia:clear_table(registered_user)
    end,
    [{"CWIGA disallows a client to join a group if they are not authorised",
      fun() ->
          Args = [{"client",Client}],
          Body = set_params(Args),
          Response = http_request(post,?URL ++ "/groups/join/" ++ Group, Body),
          ?assertEqual(401,check_response(code,Response))
     end},
     {"CWIGA allows a client to join a group",
      fun() ->
          Args = [{"client",Client}],
          Body = set_params(Args),
          Response = http_login(post,?URL ++ "/groups/join/" ++ Group, {Nick1,Password1},Body),
          ?assertEqual(200,check_response(code,Response)),
          ?assertEqual(<<"baph joined group">>,check_json(check_response(body,Response)))
      end},
     {"CWIGA gives an error if a client tries to login again",
      fun() ->
          Args = [{"client",Client}],
          Body = set_params(Args),
          Response = http_login(post,?URL ++ "/groups/join/" ++ Group, {Nick1,Password1},Body),
          ?assertEqual(500,check_response(code,Response)),
          ?assertEqual(<<"Unable to connect!">>,check_json(check_response(body,Response)))
      end},
     {"CWIGA allows clients are unable to send messages a group if they are not connected to it",
      fun() ->
          Args = [{"msg","hey"},{"client",Client2}],
          Body = set_params(Args),
          Response = http_login(post,?URL ++ "/groups/send/" ++ Group, {Nick1,Password1},Body),
          ?assertEqual(404,check_response(code,Response)),
          ?assertEqual(<<"Must join group first!">>,check_json(check_response(body,Response)))
      end},
          {"CWIGA allows clients to send messages to chatterl groups",
      fun() ->
          Args = [{"msg","hey"},{"client",Client}],
          Body = set_params(Args),
          Response = http_request(post,?URL ++ "/groups/send/" ++ Group, Body),
          ?assertEqual(401,check_response(code,Response))
      end},
     {"CWIGA allows clients to send messages to chatterl groups",
      fun() ->
          Args = [{"msg","hey"},{"client",Client}],
          Body = set_params(Args),
          Response = http_login(post,?URL ++ "/groups/send/" ++ Group, {Nick1,Password1},Body),
          ?assertEqual(200,check_response(code,Response)),
          ?assertEqual(<<"Message sent">>,check_json(check_response(body,Response)))
      end},
     {"CWIGA disallows clients from leaving a group if they are not authorised",
      fun() ->
          Args = [{"client",Client}],
          Body = set_params(Args),
          Response = http_request(post,?URL ++ "/groups/leave/" ++ Group,Body),
          ?assertEqual(401,check_response(code,Response))
          end},
     {"CWIGA allows clients are able to leave a chatterl group",
      fun() ->
          Args = [{"client",Client}],
          Body = set_params(Args),
          Response = http_login(post,?URL ++ "/groups/leave/" ++ Group, {Nick1,Password1},Body),
          Response2 = http_login(post,?URL ++ "/groups/leave/" ++ Group,{Nick1,Password1}, Body),
          ?assertEqual(200,check_response(code,Response)),
          ?assertEqual(<<"baph has disconnected from nu">>,check_json(check_response(body,Response))),
          ?assertEqual(500,check_response(code,Response2))
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
          ?assertEqual(<<"noobie's passwords must match">>,check_json(check_response(body,Response)))
      end},
     {"CWIGA allows client to register if they have the correct credentials",
      fun() ->
          Args = [{"pass2",Password1},{"pass1",Password1},{"email",Email1},{"name",Name1}],
          Body = set_params(Args),
          Response = http_request(post,?URL ++ "/register/" ++ Nick1, Body),
          ?assertEqual(200,check_response(code,Response)),
          ?assertEqual(<<"noobie is registered">>,check_json(check_response(body,Response)))
      end},
    {"CWIGA does not allows clients to register if a nick is already in use",
      fun() ->
          Args = [{"pass2",Password1},{"pass1",Password1},{"email",Email1},{"name",Name1}],
          Body = set_params(Args),
          Response = http_request(post,?URL ++ "/register/" ++ Nick1, Body),
          ?assertEqual(404,check_response(code,Response)),
          ?assertEqual(<<"noobie is already registered">>,check_json(check_response(body,Response)))
      end},
     {"CWIGA will not login clients if their credentials are not correct",
      fun() ->
          Args = [{"pass","blah"},{"login",Nick1}],
          Body = set_params(Args),
          Response = http_request(post,?URL ++ "/users/login", Body),
          ?assertEqual(500,check_response(code,Response)),
          ?assertEqual(<<"Unable to login">>,check_json(check_response(body,Response)))
      end},
     {"CWIGA allows a registered client to login to chatterl",
      fun() ->
          Args = [{"pass",Password1},{"login",Nick1}],
          Body = set_params(Args),
          Response = http_request(post,?URL ++ "/users/login", Body),
          Response2 = http_request(post,?URL ++ "/users/login", Body),
          ?assertEqual(200,check_response(code,Response)),
          ?assertEqual(<<"noobie is logged in.">>,check_json(check_response(body,Response))),
          ?assertEqual(500,check_response(code,Response2)),
          ?assertEqual(<<"Already logged in">>,check_json(check_response(body,Response2)))
      end},
     {"CWIGA alerts clients to the fact they need to be registered to login",
      fun() ->
          Args = [{"pass","blah"},{"login","blah"}],
          Body = set_params(Args),
          Response = http_request(post,?URL ++ "/users/login", Body),
          ?assertEqual(500,check_response(code,Response)),
          ?assertEqual(<<"Not registered">>,check_json(check_response(body,Response)))
      end},
      {"CWIGA will not logout a client that is not already connected",
       fun() ->
           Args = [{"client","blah"}],
           Body = set_params(Args),
           Response = http_request(post,?URL ++ "/users/logout", Body),
           ?assertEqual(500,check_response(code,Response)),
           ?assertEqual(<<"Not logged in">>,check_json(check_response(body,Response)))
      end},
      {"CWIGA will logout a client that is connected to chatterl",
       fun() ->
           Args = [{"client",Nick1}],
           Body = set_params(Args),
           Response = http_request(post,?URL ++ "/users/logout", Body),
           ?assertEqual(200,check_response(code,Response)),
           ?assertEqual(<<"noobie is logged out.">>,check_json(check_response(body,Response)))
      end}]}].



cwiga_registeration_clients_can_get_archived_messages_test_() ->
  {Nick1,Name1,Email1,Password1} = {"noobie","noobie 1","noobie@noobie.com","blahblah"},
  Sender = "baft",
  [{setup,
    fun() ->
        inets:start(),
        chatterl:start(),
        cwiga_register({Nick1,Name1,Email1,Password1}),
        http:request(?URL ++ "/users/connect/" ++ Sender)
    end,
    fun(_) ->
        chatterl:stop(),
        mnesia:clear_table(registered_user)
    end,
    [{"CWIGA disallows messages to be sent is the client is not authorised",
      fun() ->
          Args = [{"msg","hey"},{"client",Sender}],
          Body = set_params(Args),
          Response = http_request(post,?URL ++ "/users/send/" ++ Nick1, Body),
          ?assertEqual(401,check_response(code,Response))
      end},
     {"CWIGA does not allow clients to messages to clients that are not logged in or registered",
      fun() ->
          Args = [{"msg","hey"},{"client",Sender}],
          Body = set_params(Args),
          Response = http_login(post,?URL ++ "/users/send/" ++ Nick1,{Nick1,Password1}, Body),
          ?assertEqual(500,check_response(code,Response)),
          ?assertEqual(<<"noobie is not connected!">>,check_json(check_response(body,Response)))
      end},
    {"CWIGA does not allow clietns to send message if the sender does not exist",
      fun() ->
          Args = [{"msg","hey"},{"client",Sender}],
          Body = set_params(Args),
          Response = http_login(post,?URL ++ "/users/send/" ++ "blah", {Nick1,Password1},Body),
          ?assertEqual(500,check_response(code,Response)),
          ?assertEqual(<<"blah is not connected!">>,check_json(check_response(body,Response)))
      end},
    {"CWIGA allows connected clients to send messages to registered clients",
      fun() ->
          Args = [{"msg","hey"},{"client",Nick1}],
          Body = set_params(Args),
          Response = http_login(post,?URL ++ "/users/send/" ++ Sender, {Nick1,Password1},Body),
          ?assertEqual(200,check_response(code,Response)),
          ?assertEqual(<<"Sending message to noobie...">>,check_json(check_response(body,Response)))
      end}]}].

cwiga_allows_retrieval_of_registered_logged_in_clients_test_() ->
  {Nick1,Name1,Email1,Password1} = {"noobie","noobie 1","noobie@noobie.com","blahblah"},
  {Nick2,Name2,Email2,Password2} = {"nerf","nerf 1","nerf@noobie.com","asfdasdf"},
  [{setup,
    fun() ->
        inets:start(),
        chatterl:start(),
        cwiga_register({Nick1,Name1,Email1,Password1}),
        cwiga_register({Nick2,Name2,Email2,Password2})
    end,
    fun(_) ->
        chatterl:stop(),
        mnesia:clear_table(registered_user)
    end,
    [{"CWIGA only allows authorised client to list logged in clients",
      fun() ->
          Response = http:request(?URL ++ "/status/logged_in/"),
          ?assertEqual(401,check_response(code,Response)),
          ?assertEqual(<<"Need to authorize">>,check_json(check_response(body,Response)))
      end},
     {"CWIGA can retrieve an empty list of logged in clients",
      fun() ->
          Response = http_login(?URL ++ "/status/logged_in/",{Nick1,Password1}),
          ?assertEqual(200,check_response(code,Response)),
          ?assertEqual({struct,[{<<"clients">>,[]}]},check_json(check_response(body,Response)))
      end},
     {"CWIGA can retrieve a single logged in client",
      fun() ->
          chatterl_serv:login(Nick1,Password1),
          Response = http_login(?URL ++ "/status/logged_in/",{Nick1,Password1}),
          ?assertEqual(200,check_response(code,Response)),
          ?assertEqual({struct,[{<<"clients">>,[{struct,[{<<"client">>,<<"noobie">>}]}]}]},
                       check_json(check_response(body,Response)))
      end},
     {"CWIGA can retrieve a multiple list of clients",
      fun() ->
          chatterl_serv:login(Nick2,Password2),
          Response = http_login(?URL ++ "/status/logged_in/",{Nick1,Password1}),
          ?assertEqual({struct,[{<<"clients">>,[
                                                {struct,[{<<"client">>,<<"nerf">>}]},
                                                {struct,[{<<"client">>,<<"noobie">>}]}]}]},
                       check_json(check_response(body,Response)))
      end}]}].
