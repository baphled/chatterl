%%%----------------------------------------------------------------
%%% @author  Yomi Colledge <yomi@boodah.net>
%%% @doc Web interface for Chatterl
%%%
%%% Chatterl Web Interface middleman, all communication to Chatterl
%%% go via the middle man.
%%%
%%% Prepares &amp; sends messages to Chatterl Serv and manages
%%% clients connected to the interface.
%%% @end
%%% @copyright 2008-2009 Yomi Colledge
%%%---------------------------------------------------------------
-module(chatterl_mid_man).
-behaviour(gen_server).

%% API
%% Client based calls
-export([start/0,connect/2,disconnect/2,user_list/1,user_list/2,user_msg/2,user_poll/2]).

%% Group based calls
-export([group_join/2,group_leave/2,group_info/2,group_send/2,group_poll/2,group_list/1]).

%% Admin based calls
-export([group_create/2,group_drop/2]).

%% helpers
-export([build_carrier/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(carrier, {type,message}).
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc Start Chatterl's middle man process.
%%
%% @spec start() -> {ok,Pid} | ignore | {error,Error}
%% @end
%%--------------------------------------------------------------------
start() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Connects a client to the Chatterl system.
%%
%% @spec connect(Client) -> {ResponseType,Message}
%% @end
%%--------------------------------------------------------------------
connect(ContentType,Client) ->
  {Type,Reply} =
    case gen_server:call({global, ?SERVER}, {connect, Client}) of
      {ok,_Msg} ->
        {"success",Client++" now connected"};
      {error,_Error} ->
        {"failure","Unable to connect"}
    end,
  get_response_body(ContentType,build_carrier(Type,Reply)).

%%--------------------------------------------------------------------
%% @doc Disconnects a client to the Chatterl system.
%%
%% @spec disconnect(Client) -> {ResponseType,Message}
%% @end
%%--------------------------------------------------------------------
disconnect(ContentType,Client) ->
  {Type,Reply} =
    case gen_server:call({global, ?SERVER}, {disconnect, Client}) of
      {ok,Msg} ->
        {"success",Msg};
      {error,Error} ->
        {"failure",Error}
    end,
  get_response_body(ContentType,build_carrier(Type,Reply)).

%%--------------------------------------------------------------------
%% @doc Lists the clients connected to Chatterl
%%
%% @spec user_list() -> {carrier,ResponseType,Message}
%% @end
%%--------------------------------------------------------------------
user_list(ContentType) ->
  {Type,Result} =
    case gen_server:call({global,chatterl_serv},list_users) of
      [] -> {"success",build_carrier("clients","")};
      Clients ->
        ClientsList = [build_carrier("client",Client)||Client <- Clients],
        {"success",build_carrier("clients",ClientsList)}
    end,
  get_response_body(ContentType,build_carrier(Type,Result)).

%%--------------------------------------------------------------------
%% @doc List users connected to a specified group
%%
%% @spec user_list(Group) -> {carrier,ResponseType,Message}
%% @end
%%--------------------------------------------------------------------
user_list(ContentType,Group) ->
  {Type,Record} =
    case gen_server:call({global,chatterl_serv},{group_exists,Group}) of
      true -> ClientsList = [build_carrier("client",Client)
                             || {Client,{_Pid,_Ref}}
                                  <- gen_server:call({global,Group},list_users)],
              {"success",build_carrier("clients",ClientsList)};
      false ->
        {"error","Group: "++ Group ++ " doesn't exist"}
    end,
  get_response_body(ContentType,build_carrier(Type,Record)).

%%--------------------------------------------------------------------
%% @doc Allows a client to send a private message to another client.
%%
%% @spec user_msg(Sender,Client,Message) -> {carrier,ResponseType,Message}
%% @end
%%--------------------------------------------------------------------
user_msg(ContentType,{Sender, Client, Message}) ->
  {Type,Reply} =
    case gen_server:call({global,chatterl_serv},{user_exists,Sender}) of
      true ->
        case gen_server:call({global, ?MODULE}, {private_msg, Sender, Client, Message}) of
          {ok,_Msg} ->
            {"success","Sending msg..."};
          {error,Error} ->
            {"failure",Error}
        end;
      false ->
        {"failure","Client doesn't exist!"}
    end,
  get_response_body(ContentType,build_carrier(Type,Reply)).

%%--------------------------------------------------------------------
%% @doc Allows a client to retrieve private messages.
%%
%% @spec user_poll(Client) -> {carrier,ResponseType,Message}
%% @end
%%--------------------------------------------------------------------
user_poll(ContentType,Client) ->
  {Type,Result} =
    case gen_server:call({global,chatterl_serv},{user_exists,Client}) of
      true ->
        case gen_server:call({global,Client},poll_messages) of
          [] -> {"success",build_carrier("messages","")};
          Messages ->
            MessagesList = [build_carrier("message",format_messages(Message))
                            ||Message <- Messages],
            {"success",build_carrier("messages",MessagesList)}
        end;
      false ->
        {"error","Client: "++ Client ++ " doesn't exist"}
    end,
  get_response_body(ContentType,build_carrier(Type,Result)).

%%--------------------------------------------------------------------
%% @doc lists the groups on Chatterl
%%
%% @spec group_list() -> {carrier,ResponseType,Message}
%% @end
%%--------------------------------------------------------------------
group_list(ContentType) ->
  {Type,Result} =
    case gen_server:call({global, chatterl_serv}, list_groups, infinity) of
      [] -> {"success",build_carrier("groups","")};
      Groups ->
        GroupsList = [build_carrier("group",Group)||Group <- Groups],
        {"success",build_carrier("groups",GroupsList)}
    end,
  get_response_body(ContentType,build_carrier(Type,Result)).

%%--------------------------------------------------------------------
%% @doc Creates a group on Chatterl.
%%
%% @spec group_create(Group,Description) -> {carrier,ResponseType,Message}
%% @end
%%--------------------------------------------------------------------
group_create(ContentType,{Group,Description}) ->
  {Type,Result} =
    case gen_server:call({global,chatterl_serv},{create,Group,Description}) of
      {error,_Error} ->
        {"failure","Unable to create group"};
      {ok,_GroupPid} ->
        {"success","Group added"}
    end,
  get_response_body(ContentType,build_carrier(Type,Result)).

%%--------------------------------------------------------------------
%% @doc Drops a group from Chatterl.
%%
%% @spec group_drop(Group) -> {carrier,ResponseType,Message}
%% @end
%%--------------------------------------------------------------------
group_drop(ContentType,Group) ->
  {Type,Result} =
    case gen_server:call({global,chatterl_serv},{drop,Group}) of
      {error,{Error,_GroupName}} ->
        {"failure",Error};
      {ok,ResponseMsg} ->
        {"success",ResponseMsg}
    end,
  get_response_body(ContentType,build_carrier(Type,Result)).

%%--------------------------------------------------------------------
%% @doc Retrieves information on a specified Chatterl group.
%%
%% @spec group_info(Group) -> {carrier,ResponseType,Message}
%% @end
%%--------------------------------------------------------------------
group_info(ContentType,Group) ->
  {Type,Result} =
    case gen_server:call({global,chatterl_serv},{group_exists,Group}) of
      true ->
        Data = gen_server:call({global,chatterl_serv},{group_info,Group}),
        Results = [build_carrier(atom_to_list(Atom),Info)|| {Atom,Info} <- Data],
        {"success",build_carrier("groups",Results)};
      false ->
        {"error","Group doesn't exist!"}
    end,
  get_response_body(ContentType,build_carrier(Type,Result)).

%%--------------------------------------------------------------------
%% @doc Allows a client to join a Chatterl group.
%%
%% @spec group_join(Group,Client) -> {carrier,ResponseType,Message}
%% @end
%%--------------------------------------------------------------------
group_join(ContentType,{Group,Client}) ->
  {Type,Reply} =
	case gen_server:call({global,chatterl_serv},{group_exists,Group}) of
	    true ->
		 case gen_server:call({global,Group},{join,Client}) of
			{ok,_} ->
			    {"success",Client ++ " joined group"};
			{error,Error} ->
			    {"failure",Error}
		    end;
	    false ->
		{"error","Group: "++ Group ++ " doesn't exist"}
	end,
  get_response_body(ContentType,build_carrier(Type,Reply)).

%%--------------------------------------------------------------------
%% @doc Allows a client to leave a Chatterl group.
%%
%% @spec group_leave(Group,Client) -> {carrier,ResponseType,Message}
%% @end
%%--------------------------------------------------------------------
group_leave(ContentType,{Group,Client}) ->
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
  get_response_body(ContentType,build_carrier(Type,Record)).

%%--------------------------------------------------------------------
%% @doc Allows a client to send a message to a Chatterl group.
%%
%% @spec group_send(Group,Sender,Message) -> {carrier,ResponseType,Message}
%% @end
%%--------------------------------------------------------------------
group_send(ContentType,{Group,Sender,Message}) ->
    {Type,Reply} =
	case gen_server:call({global,chatterl_serv},{group_exists,Group}) of
	    true ->
		case gen_server:call({global,?MODULE},{group_msg,Sender,Group,Message}, infinity) of
		    {ok,Msg} ->
			{"success",Msg};
		    {error,Error} ->
			{"failure",Error}
		end;
	    false ->
		{"failure","User not joined"}
	end,
  get_response_body(ContentType,build_carrier(Type,Reply)).

%%--------------------------------------------------------------------
%% @doc Allows a client to retrieve messages from a Chatterl group.
%%
%% @spec group_poll(Group) -> {carrier,ResponseType,Message}
%% @end
%%--------------------------------------------------------------------
group_poll(ContentType,Group) ->
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
  get_response_body(ContentType,build_carrier(Type,Result)).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    io:format("Starting Chatterl Middle Man~n"),
    {ok, dict:new()}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({connect, Nickname}, _From, State) ->
    {Reply,NewState} =
	case dict:find(Nickname, State) of
	    error ->
		Pid = spawn(fun() ->
				    process_flag(trap_exit, true),
				    proxy_client([]) end),
		erlang:monitor(process, Pid),
		{chatterl_client:start(Nickname), dict:store(Nickname, Pid, State)};
	    {ok, _} ->
		{{error, duplicate_nick_found}, State}
    end,
    {reply,Reply,NewState};
handle_call({disconnect, Client}, _From,State) ->
    {Reply,NewState} =
	case dict:find(Client, State) of
	    error ->
		{{error,"Not connected"},State};
	    {ok, Pid} ->
		Pid ! {stop,Client},
		{{ok,"Disconnected"},dict:erase(Client, State)}
	end,
    {reply, Reply, NewState};
handle_call({group_msg, Sender, Group, Message}, _From, State) ->
    Reply = case dict:find(Sender, State) of
		error ->
		    io:format("user not connected"),
		    {error,"Unable to send msg!"};
		{ok, Pid} ->
		    Pid ! {group_msg, Sender, Group, Message},
		    {ok, "Sending group msg..."}
	    end,
    {reply, Reply, State};
handle_call({private_msg, Sender, Client, Message}, _From, State) ->
    Reply =
	case dict:find(Client, State) of
	    error ->
		io:format("user not connected"),
		{error,"Unable to connect!"};
	    {ok, Pid} ->
		Pid ! {private_msg, Sender, Client, Message},
		{ok,"Sending msg"}
	end,
    {reply, Reply, State}.
%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    io:format("Terminating Chatterl Middle Man..."),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
proxy_client(Messages) ->
    receive
	{private_msg, Sender, Client, Message} ->
	    io:format("Sending private message...~n"),
	    gen_server:call({global,Sender},{private_msg,Client,Message}),
	    proxy_client(Messages);
	{group_msg, Sender, Group, Message} ->
	    io:format("Sending group message: ~s...~n", [Message]),
	    gen_server:call({global,Group},{send_msg,Sender,Message}),
	    proxy_client(Messages);
	{stop,Client} ->
	    io:format("Proxy stopping...~s~n",[Client]),
	    chatterl_client:stop(Client);
	Other -> io:format("unknown proxy request ~s~n",[Other]),
		 proxy_client(Messages)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% Builds and returns our carrier message.
%%
%% This is used to pass around message retrieved from Chatterl.
%% @spec build_carrier(Type,Message) -> Carrier
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
      json_message(Record);
    "text/xml" ->
      io:format("~s~n",[ContentType]),
      xml_message(Record);
    _ -> json_message(build_carrier("error","Illegal content type!"))
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
%% Loops over the record carrier building the tuple structure need to
%% build our JSON.
%% @spec loop_json_carrier(Carrier) -> JSONTuple
%%
%% @end
%%--------------------------------------------------------------------
loop_json_carrier(CarrierRecord) ->
    [{struct,[{DataType,clean_message(Data)}]} || {carrier,DataType,Data} <- CarrierRecord].

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
    [{struct,[{MsgType,loop_json_carrier(Msg)}]} || {carrier,MsgType,Msg} <- CarrierRecord].

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
    tuple_to_xml(XMLTuple,[]).

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
%% @endy
%%--------------------------------------------------------------------
tuple_to_xml(XmlTuple,Prolog) ->
  lists:flatten(xmerl:export_simple([XmlTuple],xmerl_xml,[{prolog,Prolog}])).
