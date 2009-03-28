-module(test_helpers).

-export([start_client/3,start_group/2,check_response/2,check_json/1,set_params/1,http_request/3,http_login/2,headers/2]).

%% Helper functions.
start_client(Client,Group,Description) ->
  start_group(Group,Description),
  chatterl_client:start(Client).

start_group(Group,Description) ->
  chatterl:start(),
  chatterl_serv:create(Group,Description).

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

check_json(Json) ->
  {struct,[{<<"chatterl">>,{struct,[{<<"response">>,{struct,[Response]}}]}}]} = Json,
  case Response of
    {<<"success">>,Result} ->
      Result;
    {<<"failure">>,Result} ->
      Result;
    {<<"error">>,Result} ->
      Result
  end.

set_params(Args) ->
  lists:concat(lists:foldl(
                 fun (Rec, []) -> [Rec]; (Rec, Ac) -> [Rec, "&" | Ac] end,
                 [],[K ++ "=" ++ url_encode(V) || {K, V} <- Args])).

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

http_request(post,Url,Body) ->
  http:request(post, {Url, [], "application/x-www-form-urlencoded", Body}, [], []).

http_login(Url,{Login,Pass}) ->
  http:request(get, {Url, headers(Login, Pass)}, [], []).

headers(nil, nil) -> [{"User-Agent", "ChatterlTest/0.1"}];
headers(User, Pass) when is_binary(User) ->
    headers(binary_to_list(User), Pass);
headers(User, Pass) when is_binary(Pass) ->
    headers(User, binary_to_list(Pass));
headers(User, Pass) ->
    UP = base64:encode(User ++ ":" ++ Pass),
    Basic = lists:flatten(io_lib:fwrite("Basic ~s", [UP])),
    [{"User-Agent", "ChatterlTest/0.1"}, {"Authorization", Basic}].
