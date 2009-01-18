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
%%% @copyright 2008-2009 Yomi Akindayini
%%%---------------------------------------------------------------
-module(chatterl_gateway).

-behaviour(gen_server).

%% API
-export([start/1,dispatch_requests/1,tuple_to_xml/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).
%% Record used to pass our basic messages from the gateway to chatterl_web
-record(carrier, {type,message}).
-define(SERVER, ?MODULE).
-define(REQ(RP), proplists:get_value(req, RP)).
-define(PATH(RP), proplists:get_value(path, RP)).
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
    %log(Req),
    [Path|Ext] = string:tokens(Req:get(path),"."),
    Action = clean_path(Path),
    handle(Action,Ext,Req).

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
handle_call({dolog,Req,Ip},_From,State) ->
    stat_logger:log("~p ~p~n", [Ip, Req:get(path)]),
    {reply,ok,State};
handle_call(_Request, _From, State) ->
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
%% Gets the content type, used to help CWIGA to determine what format
%% to respond in.
%% @spec get_content_type(Type) -> string()
%%
%% @end
%%--------------------------------------------------------------------
get_content_type(Type) ->
    case Type of
	["json"] ->
	    "text/plain";
	["xml"] ->
	    "text/xml";
	_ -> "text/plain"
    end.

%%--------------------------------------------------------------------
%% @doc
%%
%% Handles our RESTful resquests.
%%
%% @spec handle(Action,ContentType,Req) -> void()
%%
%% @end
%%--------------------------------------------------------------------
handle("/connect/" ++ Client,ContentType,Req) ->
    {Type,Record} =
    case chatterl_mid_man:connect(Client) of
	{ok,_Msg} -> {"success",Client++" now connected"};
	{error,Error} -> {"failure",Error}
    end,
    send_response(Req,{get_content_type(ContentType),build_carrier(Type,Record)});
handle("/disconnect/" ++ Client,ContentType,Req) ->
    {Type,Record} =
    case chatterl_mid_man:disconnect(Client) of
	{ok,Msg} ->
	    {"success",Msg};
	{error,Error} ->
	    {"failure",Error}
    end,
    send_response(Req, {get_content_type(ContentType),build_carrier(Type,Record)});
handle("/users/list", ContentType ,Req) ->
    {Type,Result} =
	case gen_server:call({global,chatterl_serv},list_users) of
	    [] -> {"success",build_carrier("clients","")};
	    Clients -> 
		ClientsList = [build_carrier("client",Client)||Client <- Clients],
		{"success",build_carrier("clients",ClientsList)}
    end,
    send_response(Req,{get_content_type(ContentType),build_carrier(Type,Result)});
handle("/users/list/" ++Group,ContentType,Req) ->
    {Type,Record} = 
	case gen_server:call({global,chatterl_serv},{group_exists,Group}) of
	    true -> ClientsList = [build_carrier("client",Client)
				   || {Client,{_Pid,_Ref}} 
					  <- gen_server:call({global,Group},list_users)],
		    {"success",build_carrier("clients",ClientsList)};
	    false ->
		build_carrier("error","Group: "++ Group ++ " doesn't exist")
	end,
    send_response(Req,{get_content_type(ContentType),build_carrier(Type,Record)});
handle("/users/poll/" ++ Client,ContentType,Req) ->
    {Type,Result} = 
	case gen_server:call({global,chatterl_serv},{user_exists,Client}) of
	    true ->
		case gen_server:call({global,Client},poll_messages) of
		    [] -> {"success",build_carrier("messages","")};
		    Messages ->
			MessagesList = [build_carrier("message",format_messages(Message))||Message <- Messages],
			{"success",build_carrier("messages",MessagesList)}
		end;
	    false ->
		{"error","Client: "++ Client ++ " doesn't exist"}
	end,
    send_response(Req,{get_content_type(ContentType),build_carrier(Type,Result)});
handle("/groups/list",ContentType,Req) ->
    {Type,Result} = 
	case chatterl_mid_man:list_groups() of
	    [] -> {"success",build_carrier("groups","")};
	    Groups -> 
		GroupsList = [build_carrier("group",Group)||Group <- Groups],
		{"success",build_carrier("groups",GroupsList)}
    end,
    send_response(Req,{get_content_type(ContentType),build_carrier(Type,Result)});
handle("/groups/info/" ++ Group,ContentType,Req) ->
    {Type,Result} = 
	case gen_server:call({global,chatterl_serv},{group_exists,Group}) of
	    true ->
		
		Data = gen_server:call({global,chatterl_serv},{group_info,Group}),
		Results = [build_carrier(atom_to_list(Atom),Info)|| {Atom,Info} <- Data],
		{"success",
		 build_carrier("groups",Results)};
	    false ->
		{"error",""}
	end,
    %io:format(Result),
    send_response(Req,{get_content_type(ContentType),build_carrier(Type,Result)});
handle("/groups/join/" ++ Group,ContentType,Req) ->
    [Client] = get_properties(Req,["client"]),
    Record = 
	case gen_server:call({global,chatterl_serv},{group_exists,Group}) of
	    true ->
		 case gen_server:call({global,Group},{join,Client}) of
			{ok,_} ->
			    build_carrier("success",Client ++ " joined group");
			{error,Error} ->
			    build_carrier("failure",Error)
		    end;
	    false ->
		build_carrier("error","Group: "++ Group ++ " doesn't exist")
	end,
    send_response(Req,{get_content_type(ContentType),Record});
handle("/groups/leave/" ++ Group,ContentType,Req) ->
    [Client] = get_properties(Req,["client"]),
    {Type,Record} = 
	case gen_server:call({global,chatterl_serv},{user_exists,Client}) of
	    true ->
		case gen_server:call({global,Group},{leave,Client}) of
		    {ok, _ } ->
			{"success",Client ++ " has disconnected from " ++ Group};
		    {error,Error} ->
			{"failure",Error}
		end;
	    false ->
		{"failure","User not joined"}
	end,
    send_response(Req,{get_content_type(ContentType),build_carrier(Type,Record)});
handle("/groups/send/" ++ Group, ContentType, Req) ->
    [Client, Message] = get_properties(Req,["client","msg"]),
    {Type,Record} =
	case gen_server:call({global,chatterl_serv},{user_exists,Client}) of
	    true ->
		case gen_server:call({global,Group},{send_msg,Client,Message}, infinity) of
		    {ok,Msg} ->
			{"success",atom_to_list(Msg)};
		    {error,Error} ->
			{"failure",Error}
		end;
	    false ->
		{"failure","User not joined"}
	end,
    send_response(Req,{get_content_type(ContentType),build_carrier(Type,Record)});
handle("/groups/poll/" ++ Group,ContentType,Req) ->
    {Type,Result} = 
	case gen_server:call({global,chatterl_serv},{group_exists,Group}) of
	    true ->
		case gen_server:call({global,Group},poll_messages) of
		    [] -> {"success",build_carrier("messages","")};
		    Messages ->
			MessagesList = [build_carrier("message",format_messages(Message))||Message <- Messages],
			{"success",build_carrier("messages",MessagesList)}
		end;
	    false ->
		{"error","Group: "++ Group ++ " doesn't exist"}
	end,
    send_response(Req,{get_content_type(ContentType),build_carrier(Type,Result)});
handle("/groups/create/" ++Group,ContentType,Req) ->
    Description = mochiweb_util:shell_quote(get_properties(Req,["description"])),
    {Type,Result} = 
	case is_auth(Req) of
	    {ok,_Msg} ->
		case gen_server:call({global,chatterl_serv},{create,Group,Description}) of
		    {error,_Error} ->
			{"failure","Unable to create group"};
		    {ok,_GroupPid} ->
			{"success","Group added"}
		end;
	    {error,Error} ->
		{"error",Error}
	end,
    send_response(Req,{get_content_type(ContentType),build_carrier(Type,Result)});
handle("/groups/drop/" ++Group,ContentType,Req) ->
    {Type,Result} = 
	case is_auth(Req) of
	    {ok,_Msg} ->
		case gen_server:call({global,chatterl_serv},{drop,Group}) of
		    {error,{Error,_GroupName}} ->
			{"failure",Error};
		    {ok,ResponseMsg} ->
			{"success",ResponseMsg}
		end;
	    {error,Error} ->
		{"error",Error}
	end,
    send_response(Req,{get_content_type(ContentType),build_carrier(Type,Result)});
handle(Unknown, ContentType,Req) ->
    send_response(Req,{get_content_type(ContentType),build_carrier("error", "Unknown command: " ++Unknown)}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Gets a list of properties.
%% @spec get_properties(Req,WantedParams) -> [ParamValues]
%%
%% @end
%%--------------------------------------------------------------------
get_properties(Req,WantedParams) ->
    Params = Req:parse_qs(),
    [proplists:get_value(Property, Params)|| Property <-  WantedParams].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Builds and returns our carrier message.
%%
%% This is used to pass around message retrieved from Chatterl.
%% @spec build_carrier(Type,Message) -> Record
%%
%% @end
%%--------------------------------------------------------------------
build_carrier(Type,Message) ->
    #carrier{ type=Type, message=Message}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Generates the record for joining a Chatterl group.
%%
%% Have a feeling this can be cleaned up or used in other places, so
%% I have place it here.
%% @spec format_messages(MessageCarrier) -> [MessageRecord]
%%
%% @end
%%--------------------------------------------------------------------
format_messages({Client,Date,Message}) ->
    [build_carrier("client",Client),build_carrier("date",Date),build_carrier("msgbody",Message)].

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
    %io:format(Response),
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
	"text/plain" ->
	    Result = json_message(Record),
	    %io:format(Result),
	    Result;
	"text/xml" ->
	    xml_message(Record);
	_ -> json_message(build_carrier("error","Illegal content type!"))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Gets our response code depending on the type of message passed by 
%% the carrier record.
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
%% @spec json_message(CarrierRecord) -> JSON
%%
%% @end
%%--------------------------------------------------------------------
json_message(CarrierRecord) ->
    {carrier, CarrierType, Message} = CarrierRecord, 
    Struct =
	case Message of
	    {carrier,Type,MessagesCarrier} ->
		case Type =:= "groups" 
		    orelse Type =:= "clients" 
		    orelse Type =:= "messages" of
		    true ->
			handle_messages_json(Type,MessagesCarrier,CarrierType);
		    false ->
			io:format("dont know ~s~n",[Type])
		end;
	    _ ->
		{struct,[{CarrierType,list_to_binary(Message)}]}
	end,
    mochijson2:encode({struct,[{chatterl,{struct,[{response,Struct}]}}]}).

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
	    case Type =:= "groups" 
		orelse Type =:= "clients" 
		orelse Type =:= "messages" of
		true -> 
		    handle_messages_xml(Type,Record,MessageType);
		false -> io:format("dont know ~s~n",[Type])
	    end;
	_ -> xml_tuple_single(MessageType,Message)
    end,
    %io:format(XMLTuple),
    tuple_to_xml(XMLTuple,[]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Generates JSON structure needed to create message responses.
%%
%% As messages are a different format that the other responses, they
%% need to be handled uniquely. We also want to make sure that all three
%% clauses are meet (empty,single message, multiple messages).
%% @spec handle_messages_json(CarrierType,MessagesCarrier,Type) -> JSON
%%
%% @end
%%--------------------------------------------------------------------
handle_messages_json(Type,MessagesCarrier,CarrierType) ->
    %io:format(MessagesCarrier),
    case Type =:= "messages" of
	true ->
	    case MessagesCarrier of
		[] -> %Empty list.
		    {struct,[{Type,[]}]};
		[{carrier,_MessageType,MessageData}] ->	% A Single message.
		    {struct,[{CarrierType,
			      {struct,[{Type,loop_json_carrier(MessageData)}]}}]};
		Messages -> % Multiple messages
		    {struct,[{CarrierType,
			      {struct,[{Type,inner_loop_json_carrier(Messages)}]}}]}
	    end;
	false ->
	    Result = loop_json_carrier(MessagesCarrier),
	    {struct,[{CarrierType,{struct,[{Type,Result}]}}]}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Generates XML structure needed to create message responses.
%%
%% @spec handle_messages_xml(Type,CarrierType,MessagesCarrier) -> XMLTuple
%%
%% @end
%%--------------------------------------------------------------------
handle_messages_xml(Type,MessagesCarrier,CarrierType) ->
    case Type =:= "messages" of
	true ->
	    case MessagesCarrier of
		_ -> 
		    Result = inner_loop_xml_tuple(MessagesCarrier),
		    Data = loop_xml_tuple(Type,Result),
		    xml_tuple_single(CarrierType,Data)
	    end;
	false ->
	    Result = loop_xml_carrier(MessagesCarrier),
	    xml_tuple(Type,[Result])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Loops over the record carrier building the tuple structure need to
%% build our JSON.
%% @spec loop_json_carrier(Carrier) -> JSONTuple
%%
%% @end
%%--------------------------------------------------------------------
loop_json_carrier(CarrierRecord) ->
    [{struct,[{list_to_binary(DataType),clean_message(Data)}]} || {carrier,DataType,Data} <- CarrierRecord].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Loops over the message carrier building the tuple structure need to
%% build our JSON.
%%
%% We use this to build each message retrieved from a group, as they
%% have a number of elements we need to parse those the same as the
%% outer elements.
%% @spec inner_loop_json_carrier(CarrierRecord) -> JSONTuple
%%
%% @end
%%--------------------------------------------------------------------
inner_loop_json_carrier(CarrierRecord) ->
    [{struct,[{list_to_binary(MsgType),loop_json_carrier(Msg)}]} || {carrier,MsgType,Msg} <- CarrierRecord].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Cleans up our messages, ready for JSON/XML construction.
%%
%% We need this so that we can clean our date format, later we will
%% format dates properly but this works as a quick fix.
%% @spec clean_message(Carrier) -> XMLTuple
%%
%% @end
%%--------------------------------------------------------------------
clean_message(Data) when is_tuple(Data) ->
    list_to_binary([httpd_util:rfc1123_date(Data)]);
clean_message(Data) ->
    list_to_binary(Data).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Loops over the record carrier building the tuple structure need to
%% build out XML.
%% @spec loop_xml_carrier(Carrier) -> XMLTuple
%%
%% @end
%%--------------------------------------------------------------------
loop_xml_carrier(CarrierRecord) ->
    [loop_xml_tuple(DataType,clean_xml([Data])) || {carrier,DataType,Data} <- CarrierRecord].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Loops over the inner record carriers building the tuple structure need to
%% build out XML.
%% @spec inner_loop_xml_carrier(CarrierRecord) -> [XMLTuple]
%%
%% @end
%%--------------------------------------------------------------------
inner_loop_xml_carrier(CarrierRecord) ->
    %io:format(CarrierRecord),
    [loop_xml_tuple(DataType,clean_xml([Data])) || {carrier,DataType,Data} <- CarrierRecord].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Loops over the carrier tuple building the tuple structure need to
%% build out XML.
%% @spec inner_loop_xml_tuple(Messages) -> [XMLTuple]
%%
%% @end
%%--------------------------------------------------------------------
inner_loop_xml_tuple(Messages) ->
    [loop_xml_tuple(Type,inner_loop_xml_carrier(Message))|| {carrier,Type,Message} <- Messages].

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Cleans our XML for us, especially modifying our date to a readable format.
%% @spec clean_xml([Data]) -> XMLTuple
%%
%% @end
%%--------------------------------------------------------------------
clean_xml([Data]) when is_tuple(Data) ->
    [httpd_util:rfc1123_date(Data)];
clean_xml([Data]) ->
    [Data].

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
    case Message of
	[Data] ->
	    Data;
	Data ->
	    Data
    end,
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
    {chatterl,[],[{response,[],[{list_to_atom(Type),[],[Message]}]}]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Prepares our tuple used to generate our XML.
%% @spec loop_xml_tuple(Type,Message) -> XmlTuple
%%
%% @end
%%--------------------------------------------------------------------
loop_xml_tuple(Type,Message) when is_list(Type) ->
    {list_to_atom(Type),[],Message};
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Logs all our requests.
%% @spec log(Req) -> XmlTuple
%%
%% @end
%%--------------------------------------------------------------------
log(Req) ->
 Ip = Req:get(peer),
 gen_server:call(?MODULE, {dolog, Req, Ip}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Determines whether the client has successfully authenticated.
%%
%% Need to make this more secure.
%% @spec is_auth(Req) -> {ok,Msg}|{error,Error}
%%
%% @end
%%--------------------------------------------------------------------
is_auth(Req) ->
    case Req:get_header_value("authorization") of
	"Basic "++Base64 ->
	    Str = base64:mime_decode_to_string(Base64),
	    case string:tokens(Str, ":") of
		["admin", "pass"] ->
		    {ok,"Authorized"};
		_ ->
		    {error,"Unauthorized authorization."}
	    end;
	_ ->
	    {error,"Need to authorize"}
    end.
