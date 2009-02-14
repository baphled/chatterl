%%%----------------------------------------------------------------
%%% @author  Yomi Colledge <yomi@boodah.net>
%%% @doc Web interface for Chatterl
%%%
%%% Chatterl Message Handler
%%%
%%% Used to create our message responses for Chatterl.
%%% @end
%%% @copyright 2008-2009 Yomi Colledge
%%%---------------------------------------------------------------
-module(message_handler).

-export([get_response_body/2,build_carrier/2,format_messages/1]).

-record(carrier, {type,message}).
%%--------------------------------------------------------------------
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
    ["text/json"] ->
      json_message(Record);
    ["text/xml"] ->
      xml_message(Record);
    _ -> json_message(build_carrier("error","Illegal content type!"))
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
format_messages({Date,Client,Message}) ->
  {Type,Name} = Client,
  [build_carrier("date",Date),build_carrier(atom_to_list(Type),Name),build_carrier("msgbody",Message)].

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
            io:format("~s~n",[Message]),
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
                io:format("~s~n",[_MessageType]),
		    {struct,[{CarrierType,
			      {struct,[{Type,{struct,[{_MessageType,loop_json_carrier(MessageData)}]}}]}}]};
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
