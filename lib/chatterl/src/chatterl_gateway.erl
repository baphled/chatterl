%%----------------------------------------------------------------
%%% @author  Yomi Colledge <yomi@boodah.net>
%%% @doc Web interface for Chatterl
%%%
%%% Chatterl Web Gateway
%%%
%%% Allows Chatterl to interface with any web-based interface
%%% Using JSON and XML, sending the requests off to the chatterl_serv
%%% module.
%%% @end
%%% @copyright 2008 Yomi Akindayini
%%%---------------------------------------------------------------
-module(chatterl_gateway).

-behaviour(gen_server).

-define(OK, <<"ok">>).
%% API
-export([start/1,dispatch_requests/1,tuple_to_xml/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).
-record(messages, {client,message}).
%% Record used to pass our basic messages from the gateway to chatterl_web
-record(carrier, {type,message}).
-define(SERVER, ?MODULE).
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start(Port) -> {ok,Pid} | ignore | {error,Error}
%% @end
%%--------------------------------------------------------------------
start(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%%--------------------------------------------------------------------
%% @doc
%% Dispatches our requests to the relevant handle.
%%
%% Uses clean_path to determine what the action is.
%% @spec dispatch_requests(Request) -> void()
%% @end
%%--------------------------------------------------------------------
dispatch_requests(Req) ->
  Path = Req:get(path),
  Action = clean_path(Path),
  handle(Action, Req).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initiates the server
%%
%% Function: init(Port) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}

%%--------------------------------------------------------------------
init([Port]) ->
    io:format("Initialising Chatterl Web Interface~n"),
    process_flag(trap_exit, true),
    mochiweb_http:start([{port, Port}, {loop, fun dispatch_requests/1}]),
    erlang:monitor(process,mochiweb_http),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @doc
%% Description: Handling call messages
%%
%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Rquest, _From, State) ->
    {reply,ok,State}.
    
%%--------------------------------------------------------------------
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'DOWN', _Ref, _Process, {mochiweb_http, Host}, Reason}, State) ->
    io:format("Unable to start mochiweb on ~s:~nReason: ~s~n",[Host,Reason]),
    {stop,normal,State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc
%% Terminates Chatterl Web Interface, making sure to shutdown mochiweb
%% along side it.
%%
%% @spec terminate({node,Reason},State) -> void()
%% @todo Needs a time out for when the port is already in use.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    io:format("Shutting down ChatterlWeb on: ~s...~n",[node(self())]),
    mochiweb_http:stop(),
    ok.

%%--------------------------------------------------------------------
%% @doc
%%
%% Convert process state when code is changed
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%%
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @doc
%%
%% Handles our RESTful resquests.
%% @spec handle(Action, Req) -> {ok, NewState}
%%
%% @end
%%--------------------------------------------------------------------
%% handle("/send", Req) ->
%%   Params = Req:parse_qs(),
%%   Sender = proplists:get_value("nick", Params),
%%   Group = proplists:get_value("to", Params),
%%   Message = proplists:get_value("msg", Params),
%%   chatterl_man:send_message(Sender, Group, Message),
%%   success(Req, {"text/plain",?OK});
%% Need to refactor so the request goes to chatterl_serv
handle("/connect/" ++ Client,Req) ->
    ContentType = "text/xml",
    Record = 
	case gen_server:call({global,chatterl_serv},{connect,Client}) of
	    {ok,_} ->
		get_record("success",Client ++ " now connected");
	    {error,Error} ->
		get_record("fail",Error)
	end,
    send_response(Req,{ContentType,Record});
handle("/users/list",Req) ->
    Result =
	case gen_server:call({global,chatterl_serv},list_users) of
	    [] -> get_record("users","");
	    Users -> 
		UsersList = [get_record("user",User)||User <- Users],
		get_record("users",UsersList)
    end,
    send_response(Req,{"text/xml", Result});
handle("/groups/list",Req) ->
    Result = 
	case gen_server:call({global,chatterl_serv},list_groups) of
	    [] -> get_record("groups","");
	    Groups -> 
		GroupsList = [get_record("group",Group)||Group <- Groups],
		get_record("groups",GroupsList)
    end,
    send_response(Req,{"text/xml", Result});
handle(Unknown, Req) ->
    send_response(Req,{"text/xml",get_record("error", "Unknown command: " ++Unknown)}).
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Builds and returns our carrier message.
%% @spec get_record(Type,Message) -> Record
%%
%% @end
%%--------------------------------------------------------------------
get_record(Type,Message) ->
    #carrier{ type=Type, message=Message}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Cleans up a request so we only retrieve the path.
%% @spec clean_path(Path) -> [Path]
%%
%% @end
%%--------------------------------------------------------------------
clean_path(Path) ->
    case string:str(Path, "?") of
	0 ->
	    Path;
	N ->
	    string:substr(Path, 1, string:len(Path) - (N + 1))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Handles our all responses.
%%
%% Sends responses based on the content type, which are JSON and XML.
%% @spec send_response(Req,Body) -> tuple()
%%
%% @end
%%--------------------------------------------------------------------
send_response(Req, {ContentType,Record}) when is_list(ContentType) ->
    Response = get_response_body(ContentType,Record),
    Code = get_response_code(Record),
    %% If we cant construct the code or have an illegal content type, we need to
    %% send an illegal method message back to the client.
    Req:respond({Code, [{"Content-Type", ContentType}], list_to_binary(Response)}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Gets the actual response body to return to the client.
%%
%% Responses are either in JSON or XML, if the wrong content type is passed
%% the client will recieve an error in XML format.
%% @spec get_response_body(ContentType,Record) -> [ResponseBody]
%%
%% @end
%%--------------------------------------------------------------------
get_response_body(ContentType,Record) ->
    case ContentType of
	"text/json" ->
	    to_json(Record);
	"text/xml" ->
	    xml_message(Record);
	_ -> xml_message(get_record("error","Illegal content type!"))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Gets our response code depending on the type of message passed by 
%% the carrier record.
%%
%% 
%% @spec get_response_code(Record) -> integer()
%%
%% @end
%%--------------------------------------------------------------------
get_response_code(Record) ->
    case Record of
	{carrier,Type,_Message} ->
	    case Type of
		"fail" -> 500;
		"success" -> 200;
		"error" -> 200;
		_ -> 500
	    end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Converts our carrier Record into a JSON format.
%% @spec to_json(Record) -> JSON
%%
%% @end
%%--------------------------------------------------------------------
to_json(Record) ->
    case Record of
	{carrier, Type, Message} ->
	    mochijson:encode({array, [Type,Message]});
	_ -> io:format("Illegal message~n")
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Generatese out actual XML message.
%%
%% Takes the record and converts it into a tuple which can be further
%% converted into a valid XML format using tuple_to_xml.
%% {carrier,"groups",[{carrier,"group","nu"},{carrier,"group","nu2"}]}

%% @spec xml_message(Record) -> XML
%%
%% @end
%%--------------------------------------------------------------------
xml_message(CarrierRecord) ->
    {carrier, MessageType, Message} = CarrierRecord,
    case Message of
	      {carrier, Type, Record} ->		
		  case Type of
		      "groups" ->
			  RecordList = loop_carrier(Record),
			  tuple_to_xml(xml_tuple(Type,RecordList),[]);
		      "error" ->
			  tuple_to_xml(xml_tuple(Type,Record),[]);
		      _ -> io:format("dont know ~s~n",[Type])
		  end;
	      _ -> tuple_to_xml(xml_tuple(MessageType,Message),[])
	  end.

loop_carrier(CarrierRecord) ->
    TempData = [result_xml_tuple(DataType,Data) || {carrier,DataType,Data} <- CarrierRecord],
    [ResultList] = TempData,
    ResultList.
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Prepares our tuple used to generate our XML.
%% @spec xml_tuple(Type,Message) -> {XmlBody}
%%
%% @end
%%--------------------------------------------------------------------
xml_tuple(Type,Message) ->
    {chatterl,[],[{message,[],[{list_to_atom(Type),[],[Message]}]}]}.

result_xml_tuple(Type,Message) ->
    {list_to_atom(Type),[],[Message]}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts a tuple into XML.
%% 
%% @spec tuple_to_xml(Xml,Prolog) -> [XML]
%%
%% @end
%%--------------------------------------------------------------------
tuple_to_xml(XmlTuple,Prolog) ->
  lists:flatten(xmerl:export_simple([XmlTuple],xmerl_xml,[{prolog,Prolog}])).
