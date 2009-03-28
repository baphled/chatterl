-module(test_helpers).

-export([start_client/3,start_group/2,check_response/2,check_json/1]).

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
