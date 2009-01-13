%%----------------------------------------------------------------
%%% @author  Yomi Colledge <yomi@boodah.net>
%%% @doc Web interface for Chatterl
%%%
%%% Chatterl Web Gateway, allowing Web based clients to interact
%%% with Chatterl over a RESTful API.
%%%
%%% Allows Chatterl to interface with any web-based interface
%%% Using JSON and XML, sending the requests off to the chatterl_serv
%%% module.
%%% 
%%% All calls to CWIGA will only be allowed via a specified IP, which
%%% will be defined with the configuration file.
%%% @end
%%% @copyright 2008 Yomi Akindayini
%%%---------------------------------------------------------------
-module(chatterl_gateway).

-behaviour(gen_server).

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
%%
%% @spec handle(Action, Req) -> void()
%%
%% @end
%%--------------------------------------------------------------------
handle("/send", Req) ->
    Params = Req:parse_qs(),
    Sender = proplists:get_value("client", Params),
    Group = proplists:get_value("group", Params),
    Message = proplists:get_value("msg", Params),
    Record = 
	case gen_server:call(Sender,{group_msg,Group,Message}, infinity) of
	    {ok,Msg} -> 
		get_record("success",Msg);
	    {error,Error} ->
		get_record("failure",Error)
	end,
    send_response(Req,{"text/xml",Record});
handle("/connect/" ++ Client,Req) ->
    ContentType = "text/xml",
    case gen_server:call({global,chatterl_serv},{connect,Client}) of
	{ok,_} ->
	    %SessionId = generate_session_id(),
	    %Cookie1 = mochiweb_cookies:cookie("sid", SessionId, [{path, "/"}]),
	    %% Want to assign both, will need to work out how.
	    send_response(Req,{ContentType,get_record("success",Client++" now connected")});
	{error,Error} ->
	    send_response(Req,{ContentType,get_record("failure",Error)})
    end;
handle("/disconnect/" ++ Client,Req) ->
    ContentType = "text/xml",
    case gen_server:call({global,chatterl_serv},{disconnect,Client}) of
	{ok,Message} ->
	    send_response(Req, {ContentType,get_record("success",Message)});
	{error,Error} ->
	    send_response(Req,{ContentType,get_record("failure",Error)})
    end;
handle("/users/list",Req) ->
    {Type,Result} =
	case gen_server:call({global,chatterl_serv},list_users) of
	    [] -> {"success",get_record("users","")};
	    Users -> 
		UsersList = [get_record("user",User)||User <- Users],
		{"success",get_record("users",UsersList)}
    end,
    send_response(Req,{"text/xml",get_record(Type,Result)});
handle("/groups/list",Req) ->
    {Type,Result} = 
	case gen_server:call({global,chatterl_serv},list_groups) of
	    [] -> {"success",get_record("groups","")};
	    Groups -> 
		GroupsList = [get_record("group",Group)||Group <- Groups],
		{"success",get_record("groups",GroupsList)}
    end,
    send_response(Req,{"text/xml",get_record(Type,Result)});
handle("/groups/join/" ++ Group,Req) ->
    ContentType = "text/xml",
    Params = Req:parse_qs(),
    Client = proplists:get_value("client", Params),
    Name = case Client of
	       undefined -> atom_to_list(Client);
	       _ -> Client
	   end,
    Record = 
	case gen_server:call({global,chatterl_serv},{group_exists,Group}) of
	    true ->
		generate_record(Group,Client);
	    false ->
		get_record("error","Group: "++ Group ++ " doesn't exist")
	end,
    send_response(Req,{ContentType,Record});
handle(Unknown, Req) ->
    send_response(Req,{"text/xml",get_record("error", "Unknown command: " ++Unknown)}).

generate_record(Group,Client) ->
    case gen_server:call({global,chatterl_serv},{group_exists,Group}) of
	true ->
	    case gen_server:call({global,chatterl_serv},{user_exists,Client}) of
		true ->
		    case gen_server:call({global,Group},{join,Client}) of
			{ok,_} ->
			    get_record("success",Client ++ " joined group");
			{error,Error} ->
			    get_record("failure",Error)
		    end;
		false ->
		    Name = 
			case Client of
			    undefined -> atom_to_list(Client);
			    _ -> Client
			end,
		    get_record("error","Client:" ++Name ++" doesn't exist")
	    end;
	false ->
	    get_record("error","Group: "++ Group ++ " doesn't exist")
    end.
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
%% @spec send_response(Req,{ContentType,Record}) -> void()
%%
%% @end
%%--------------------------------------------------------------------
send_response(Req, {ContentType,Record}) when is_list(ContentType) ->
    Response = get_response_body(ContentType,Record),
    Code = get_response_code(Record),
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
		"failure" -> 200;
		"success" -> 200;
		"error" -> 500;
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
%% Generates out actual XML message.
%%
%% Takes the record and converts it into a tuple which can be further
%% converted into a valid XML format using tuple_to_xml.
%% Example of XML tuple structure.
%% <code>{carrier,"groups",[{carrier,"group","nu"},{carrier,"group","nu2"}]}</code>
%%
%% @spec xml_message(Record) -> XML
%%
%% @end
%%--------------------------------------------------------------------
xml_message(CarrierRecord) ->
    {carrier, MessageType, Message} = CarrierRecord,
    XMLTuple = case Message of
	{carrier, Type, Record} ->		
	    case Type of
		"groups" ->
		    RecordList = loop_carrier(Record),
		    xml_tuple(Type,RecordList);
		"users" ->
		    RecordList = loop_carrier(Record),
		    xml_tuple(Type,RecordList);
		_ -> io:format("dont know ~s~n",[Type])
	    end;
	_ -> xml_tuple_single(MessageType,Message)
    end,
    tuple_to_xml(XMLTuple,[]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Loops over the record carrier building the tuple structure need to
%% build out XML.
%% @spec loop_carrier(Carrier) -> XMLTuple
%%
%% @end
%%--------------------------------------------------------------------
loop_carrier(CarrierRecord) ->
    Result = [loop_xml_tuple(DataType,Data) || {carrier,DataType,Data} <- CarrierRecord],
    Response = [Result],
    Response.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Prepares our tuple used to generate our XML.
%% @spec xml_tuple(Type,Message) -> {XmlBody}
%%
%% @end
%%--------------------------------------------------------------------
xml_tuple(Type,Message) when is_list(Message) ->
    [Data] = Message,
    {chatterl,[],[{response,[],[{list_to_atom(Type),[],Data}]}]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Prepares our tuple used to generate our sinlge XML elements.
%% @spec xml_tuple_single(Type,Message) -> {XmlBody}
%%
%% @end
%%--------------------------------------------------------------------
xml_tuple_single(Type,Message) ->
    {chatterl,[],[{message,[],[{list_to_atom(Type),[],[Message]}]}]}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Prepares our tuple used to generate our XML.
%% @spec loop_xml_tuple(Type,Message) -> XmlTuple
%%
%% @end
%%--------------------------------------------------------------------
loop_xml_tuple(Type,Message) ->
    {list_to_atom(Type),[],[Message]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts a tuple into XML.
%%
%% Inspired by the below link.
%% <a target="_blank" href="http://arandomurl.com/post/Simple-XML-in-Erlang">Link</a>
%% @spec tuple_to_xml(Xml,Prolog) -> [XML]
%%
%% @end
%%--------------------------------------------------------------------
tuple_to_xml(XmlTuple,Prolog) ->
  lists:flatten(xmerl:export_simple([XmlTuple],xmerl_xml,[{prolog,Prolog}])).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Strips out whitespaces out of out XML tuple.
%%
%% Extracted from the link below.
%% <a target="_blank" href="http://arandomurl.com/post/Simple-XML-in-Erlang">Link</a>
%% @spec strip_whitespace(XMLTuple) -> [XML]
%%
%% @end
%%--------------------------------------------------------------------
strip_whitespace({El,Attr,Children}) ->
  NChild = lists:filter(fun(X) ->
    case X of
    " " -> false;
    _   -> true
    end
  end,Children),
  Ch = lists:map(fun(X) -> strip_whitespace(X) end,NChild),
  {El,Attr,Ch}.
