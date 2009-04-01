%%%----------------------------------------------------------------
%%% @author  Yomi Akindayini <yomi@boodah.net>
%%% @doc Test helper functions used to help with our test cases
%%%
%%% @end
%%% @copyright 2009 Yomi Akindayini <yomi@boodah.net>
%%%----------------------------------------------------------------
-module(test_helpers).

-export([start_client/3,
         start_group/2,
         check_response/2,
         check_json/1,
         set_params/1,
         http_request/3,
         cwiga_register/1,
         cwiga_login/2,
         cwiga_request/2,
         http_login/2,
         headers/2]).

-define(URL,"http://127.0.0.1:9000").
%% Helper functions.
%%--------------------------------------------------------------------
%% @doc
%% Sets up a clients & group process
%%
%% @spec start_client(Client,Group,Description) -> ok
%% @end
%%--------------------------------------------------------------------
start_client(Client,Group,Description) ->
  start_group(Group,Description),
  chatterl_client:start(Client).

%%--------------------------------------------------------------------
%% @doc
%% Sets up a  group process
%%
%% @spec start_group(Group,Description) -> ok
%% @end
%%--------------------------------------------------------------------
start_group(Group,Description) ->
  chatterl:start(),
  chatterl_serv:create(Group,Description).

%%--------------------------------------------------------------------
%% @doc
%% Sets up a clients & group process
%%
%% @spec check_response(Check,Response) -> HTMLBody
%% @end
%%--------------------------------------------------------------------
check_response(Check,Response) ->
  {ok,{{"HTTP/1.1",Code,Status},
     [_Date,
      _Server,
      _ContentLength,
      ContentType],
     Body}} = Response,
  case Check of
    code ->  Code;
    status -> Status;
    content_type -> ContentType;
    body -> Body
  end.

%%--------------------------------------------------------------------
%% @doc
%% Checks our JSON response
%%
%% Used to help shorten our results
%% @spec check_json(Json) -> JSONResponse
%% @end
%%--------------------------------------------------------------------
check_json(Json) ->
  {struct,[{<<"chatterl">>,{struct,[{<<"response">>,{struct,[Response]}}]}}]} = mochijson2:decode(Json),
  case Response of
    {<<"success">>,Result} ->
      Result;
    {<<"failure">>,Result} ->
      Result;
    {<<"error">>,Result} ->
      Result;
    {<<"unauth">>,Result} ->
      Result
  end.

%%--------------------------------------------------------------------
%% @doc
%% Sets up POST parameters
%%
%% @spec set_params(Args) -> [HTTPParams]
%% @end
%%--------------------------------------------------------------------
set_params(Args) ->
  lists:concat(lists:foldl(
                 fun (Rec, []) -> [Rec]; (Rec, Ac) -> [Rec, "&" | Ac] end,
                 [],[K ++ "=" ++ url_encode(V) || {K, V} <- Args])).

%%--------------------------------------------------------------------
%% @doc
%% Encode URL
%%
%% @spec url_encode([H|T]) -> [URL]
%% @end
%%--------------------------------------------------------------------
url_encode([H|T]) ->
    if
        H >= $a, $z >= H ->
            [H|url_encode(T)];
        H >= $A, $Z >= H ->
            [H|url_encode(T)];
        H >= $0, $9 >= H ->
            [H|url_encode(T)];
        H == $_; H == $.; H == $-; H == $/; H == $: ->
            [H|url_encode(T)];
        true ->
            case integer_to_hex(H) of
                [X, Y] ->
                    [$%, X, Y | url_encode(T)];
                [X] ->
                    [$%, $0, X | url_encode(T)]
            end
     end;
url_encode([]) -> [].

%%--------------------------------------------------------------------
%% @doc
%% Converts integers to hexidecimal
%%
%% @spec integer_to_hex(I]) -> [URL]
%% @end
%%--------------------------------------------------------------------
integer_to_hex(I) ->
    case catch erlang:integer_to_list(I, 16) of
        {'EXIT', _} ->
            old_integer_to_hex(I);
        Int ->
            Int
    end.

old_integer_to_hex(I) when I<10 ->
    integer_to_list(I);
old_integer_to_hex(I) when I<16 ->
    [I-10+$A];
old_integer_to_hex(I) when I>=16 ->
    N = trunc(I/16),
    old_integer_to_hex(N) ++ old_integer_to_hex(I rem 16).

%%--------------------------------------------------------------------
%% @doc
%% Makes HTTP POST requests
%%
%% @spec http_request(post,Url,Body) -> [HTTPResponse]
%% @end
%%--------------------------------------------------------------------
http_request(post,Url,Body) ->
  http:request(post, {Url, [], "application/x-www-form-urlencoded", Body}, [], []).

http_login(Url,{Login,Pass}) ->
  http:request(get, {Url, headers(Login, Pass)}, [], []).

cwiga_request(Url,Args) ->
  case Args of
    [] ->
      http:request(Url);
    _ ->
      Body = set_params(Args),
      http_request(post,Url, Body)
  end.

cwiga_register({Nick,Name,Email,Password}) ->
  Args = [{"pass2",Password},{"pass1",Password},{"email",Email},{"name",Name}],
  Body = set_params(Args),
  http_request(post,?URL ++ "/register/" ++ Nick, Body).

cwiga_login(Login,Pass) ->
  Args = [{"pass",Pass},{"login",Login}],
  cwiga_request(?URL ++ "/users/login",Args).
%%--------------------------------------------------------------------
%% @doc
%% Sets up our HTTP headers
%%
%% @spec headers(Login,Pass) -> [HTTPAuth]
%% @end
%%--------------------------------------------------------------------
headers(nil, nil) -> [{"User-Agent", "ChatterlTest/0.1"}];
headers(User, Pass) when is_binary(User) ->
    headers(binary_to_list(User), Pass);
headers(User, Pass) when is_binary(Pass) ->
    headers(User, binary_to_list(Pass));
headers(User, Pass) ->
    UP = base64:encode(User ++ ":" ++ Pass),
    Basic = lists:flatten(io_lib:fwrite("Basic ~s", [UP])),
    [{"User-Agent", "ChatterlTest/0.1"}, {"Authorization", Basic}].
