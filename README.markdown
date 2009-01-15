<h1>The chatterl application</h1>
<p>Copyright &copy; 2008-2009 Yomi Colledge</p>
<p><b>Version:</b> Jan 15 2009 01:45:25</p>
<p><b>Authors:</b> Yomi Colledge (<a href="baphled.wordpress.com"><tt>baphled</tt></a>).</p>

<ul>
	<li><a href="#Description">Description</a></li>
	<li><a href="#Installing_Chatterl">Installing Chatterl</a></li>
	<li><a href="#Running_Chatterl">Running Chatterl</a></li>
	<li><a href="#Features">Features</a></li>
	<li><a href="#Future_Features">Future Features</a></li>
	<li><a href="#Useage">Useage</a></li>
	
</ul>

<h3><a name="Description">Description</a></h3><p>
A multi processed chat system that can be housed over a number of nodes &amp; track clients over varying devices, at the time of the writing the system works over multiple nodes &amp; is able to do the basics (connect to a group, send message to other clients &amp; connected groups).</p>

<p>The main focus of this project is to create a chat system that is highly reliable as well as scaleable. Other developers will be able to create add-on modules that are able to interact with chatterl &amp; further enhance the functionality of chatterl &amp; the chatterl clients experience.</p>

<h3><a name="Installing_Chatterl">Installing Chatterl</a></h3>
As mentioned the system is OTP based, of which it uses Sinan to maintain its builds. At the moment of this writing Chatterl is still in alpha so their is no real release at the moment, to get it running you will need to do the following:
<pre><code>git-clone git://github.com/baphled/chatterl.git &amp;&amp;
cd chatterl &amp;&amp;
sinan doc &amp;&amp; 
sinan dist &amp;&amp;
cd _/build/development/tar &amp;&amp;
sudo faxien install-release chatterl-0.1.1.0.tar.gz</code></pre>

<p>The above presumes that you have Sinan configured &amp; installed, if you haven't refer to erlware.
You will need to change to cookie &amp; name values to something else, doing so should drop you into the erlang shell &amp; ready to run the Chatterl application. </p>

<h3><a name="Running_Chatterl">Running Chatterl</a></h3>
To run a client on a different machine you will need to do the above if  (making sure that the -sname is not the same as any other connected nodes &amp; that the cookie is the same) if connecting on the same box simply cd to the ebin directory &amp; run the following command:
<pre><code>erl -s chatterl -s reloader -name bar -setcookie abc</code></pre><p>
Bar being the name you want use to identify the erlang node.</p>

From here you will need to make sure that they nodes can connect using the below command from another node:
<pre><code>net_adm:ping(foo@bar.net).</code></pre>

<p>Where foo is the node name &amp; bar.net is the tld of the node box (indicated within yout hosts file or dns server).
Once you receive the infamous pong response you are ready to roll.</p>

<h3><a name="Features">Features</a></h3>
<ul>
    <li>Client login/logout to Chatterl.</li>
    <li>List Chatterl groups.</li>
    <li> List Chatterl users.</li>
    <li> Login/logout of a chatterl group.</li>
    <li> Send message to a group &amp; other clients.</li>
    <li>A RESTful API</li>
</ul>

<h3><a name="Future_Features">Future Features</a></h3>
<ul>
    <li> Centralised Error logging &amp; data storage.</li>
    <li> Client customisable routines (able to poll RSS feeds, twitter, FB &amp; the such like).</li>
    <li> Better handling of errors.</li>
    <li> User registration.</li>
    <li> FB Connect.</li>
    <li> Chat bots (AIML based).</li>
    <li> Web frontend (using BeepBeep).</li>
    <li> Chat modules handler(banning, censorship, chatbots).</li>
</ul>

<h3><a name="Useage">Useage</a></h3>
<ul>
	<li><a href="#Shell_Interaction">Shell Interaction</a></li>
	<li><a href="#CWIGA">CWIGA</a></li>
</ul><p>
At the moment of this writing there are two ways to interact with Chatterl, through a Erlang shell or via CWIGA, which gives you the ability to interact with Chatterl via a RESTful API.</p>

<h3><a name="Shell_Interaction">Shell Interaction</a></h3>

<b>Starting the server</b>
<pre><code>erl -s chatterl</code></pre><p>
Chatterl server runs as an OTP application &amp; uses a supervisor to manage it (in later versions there will be options to spawn multiple servers, allowing for a more fault tolerant chat system). To start up the server you simply need to run the following command:</p>


<p>Which will initialise the server &amp; CWIGA The backend allowing clients to connect &amp; groups to be created &amp; admin the ability to manage the system. Groups can be created on differing nodes as long as the node can communicate with the chatterl_serv.</p>

<p>CWIGA allows for developers to interact with the API, giving them the ability use the basic CRUD functionalities of Chatterl as well as handle clients along with thier messages &amp; other functionality.</p>

<b>Starting a group</b>
<pre><code>chatterl_serv:create("room","description").</code></pre><p>
Which will spawn a group process which users can connect to.</p>

<p>Chatterl groups can be started on any node that can communicate with the server, this allows the user to create a number of groups on varying nodes, helping with general organisation as well a performance &amp; reliablity.</p>

<b>Connection to chatterl</b>
<pre><code>chatter_client:start(UserName).</code></pre><p>
Creating a connection to the server is done by using the following command.</p>

<p>At the time of this writing chatterl_clients can only spawn a client per node, this will later be changed once the web interface has been fully implemented, possibly to a refactoring the client to a parameterised module.
For the moment node users must follow the basic OTP configurations (same cookie, valid DNS name, etc).</p>

<p>This will initialise a user &amp; connect them to chatterl_serv (must be done before users can join a group or communicate with other chatterl users).</p>

<b>Disconnecting from chatterl</b> 
<pre><code>chatterl_client:stop().</code></pre>

<p>This will disconnect the user from all the groups they are currently connected to as well as the actual Chatterl server.</p>

<b>Joining a group</b>
<pre><code>chatterl_client:join(GroupName).</code></pre><p>
If the group exists the user is able to join the group allowing them to send message to the room.</p>

<b>Dropping from a group</b>
<pre><code>chatterl_client:drop(GroupName).</code></pre><p>
This will send a message to the group, which will handle the termination.</p>

<b>Sending group message</b>
<pre><code>chatterl_client:send_msg(GroupName,Message).</code></pre><p>
GroupName being the name of the group the client is connected to, Message being the message that you want to send to the receiving client. If the message is sent successfully all users connected to the group will receive the message.</p>

<b>Sending a private message</b>
<pre><code>chatterl_client:private_msg(RecipientName,Message).</code></pre><p>
This allows a Chatterl client to send a private message to another client.</p>

If the message is sent successfully then the sender will receive follow message:
<pre><code>{ok,msg_sent}</code></pre>

<p>in turn sending the message to the receipients node.</p>

<h3><a name="CWIGA">CWIGA</a></h3>
<ul>
	<li><a href="#CWIGA_Brief">CWIGA Brief</a></li>
	<li><a href="#Response_Types">Response Types</a></li>
	<li><a href="#Response_Structures">Response Structures</a></li>
	<li><a href="#CWIGA_Calls">CWIGA Calls</a></li>
</ul>

<h3><a name="CWIGA_Brief">CWIGA Brief</a></h3><p>
CWIGA handles all interaction with Chatterl, though for the moment the basics have only been implemented. It can respond in both XML &amp; JSON (at the moment of  writing only XML is functional).</p>

CWIGA responsed coming in a standardised structure, allowing for easy parsing and searching of data, all data is represented in the following formats:
<ul>
	<li>JSON</li>
	<li>XML</li>
</ul>

Any unrecognised formats will fall back to JSON, as it is the default response format. Defining the prefered format can been done by appending the corresponding path extension to the query string <pre><code>http://CWIGAURL:9000/users/some_group/list.xml</code></pre><p> will retrieve the user list of some_group in XML format.</p>

<h3><a name="Response_Types">Response Types</a></h3>
CWIGA has three types of responses which are as follows:
<ul>
	<li>Success</li>
	<li>Failure</li>
	<li>Error</li>
</ul>

<p><b>Success</b>
When ever a response has been successfully retrieved from Chatter, returning with a 200 HTTP response code along with the corresponding response structure (see <a href="#Response_Structures">Response Structures</a>).</p>

<p><b>Failure</b>
When a Chatterl responds with a failure (user cannot connect), CWIGA in turn responds with failure, returning with a 200 HTTP response code.</p>

<p><b>Error</b>
If some kind of error occurs within Chatterl CWIGA responds with an error. As these are usually internal errors, they return with a 500 HTTP response code.</p>

<h3><a name="Response_Structures">Response Structures</a></h3>
All CWIGA responses follow the same format for simplicity &amp; ease of use. There are three basic response format that can be retrieved from CWIGA:
<ul>
	<li>Empty</li>
	<li>Populated</li>
	<li>Messages</li>
</ul>

<p>Following are examples of the different response types shown in JSON &amp; XML respectively.</p>

<p><b>Empty</b>
Empty responses are structured are used when ever a response is returned with an empty value, these response are displayed in the below formats:</p>

<pre><code>&lt;chatterl&gt;
	&lt;response&gt;
		&lt;users/&gt;
	&lt;/response&gt;
&lt;/chatterl&gt;</code></pre>

<pre><code>{"chatterl":
	{"response":
		{"success":
			{"groups":[]}
		}
	}
}</code></pre>

<p><b>Message list</b>
Populated lists are have the following structure:
When CWIGA has a number of results it creates a structure simular to below:</p>

<pre><code>{"chatterl":
	{"response":
		{"success":
			{"groups":
				[
					{"group":"nu"},
					{"group":"another group"}
				]
			}
		}
	}
}</code></pre>

<pre><code>&lt;chatterl&gt;
	&lt;response&gt;
		&lt;users&gt;
			&lt;user&gt;noobie&lt;/user&gt;
			&lt;user&gt;noobiz&lt;/user&gt;
			&lt;user&gt;nooby&lt;/user&gt;
			&lt;user&gt;noobz&lt;/user&gt;
		&lt;/users&gt;
	&lt;/response&gt;
&lt;/chatterl&gt;</code></pre>

<p><b>Multi Message lists</b>
Primarily this format is used for storing all messages associated to a group (At the time of this writing this structure has not been implemented in XML format).</p>

<pre><code>{"chatterl":
	{"response":
		{"success":
			{"messages":
				[{"message":
					[
						{"client":"baph"},
						{"date":ISODATE},
						{"msgbody":"hey"}
					]
				},
				{"message":
					[
						{"client":"baph"},
						{"date":ISODATE},
						{"msgbody":"welcome"}]
				}]
			}
		}
	}
}</code></pre>

<pre><code>&lt;chatterl&gt;
	&lt;response&gt;
		&lt;messages&gt;
			&lt;message&gt;
				&lt;client&gt;baph&lt;/client&gt;
				&lt;date&gt;ISODATE&lt;/date&gt;
				&lt;msgbody&gt;hey&lt;/msgbody&gt;	
			&lt;/message&gt;
			&lt;message&gt;
				&lt;client&gt;baph&lt;/client&gt;
				&lt;date&gt;ISODATE&lt;/date&gt;
				&lt;mgsbody&gt;welcome&lt;/msgbody&gt;	
			&lt;/message&gt;
		&lt;/messages&gt;
	&lt;/response&gt;
&lt;/chatterl&gt;</code></pre>

<b>Messages</b>
There are a couple of reasons for receiving messages, the first two being used for success &amp; failures (which are used for simple interactions with CWIGA). The other time is when CWIGA has come across an error, messages are formatted in the following formats:
<pre><code>{"chatterl":
	{"response":
		{"success":"noob now connected"}
	}
}</code></pre>

<pre><code>
&lt;chatterl&gt;
	&lt;response&gt;
		&lt;failure&gt;Unable to connect.&lt;/failure&gt;
	&lt;/response&gt;
&lt;/chatterl&gt;
</code></pre>

<h3><a name="CWIGA_Calls">CWIGA Calls</a></h3><p>
All commands apart from connect require the client to connect, without it, they will receive an erorr (at the moment this is not true).</p>

CWIGA commands are as follows:
<ul>
	<li>Connect to Chatterl.</li>
	<li>Disconnect to Chatterl.</li>
	<li>List Users.</li>
	<li>List Groups.</li>
	<li>List Groups Users.</li>
	<li>Join a Group.</li>
	<li>Leave a Group.</li>
	<li>Send Group messages.</li>
	<li>Poll Group messages.</li>
</ul>

<b>Connect to Chatterl</b>
<pre><code>http://CWIGAURL:9000/connect/USER</code></pre>
Calls Chatterl &amp; makes a connection, once this is successful, the client is able to interact with the rest of the API.
When a client connects successfully, the following response will be received:
<pre><code>{"chatterl":
	{"response":
		{"success":"foo has connected to Chatterl"}
	}
}</code></pre>

<pre><code>&lt;chatterl&gt;
	&lt;response&gt;
		&lt;success&gt;foo now connected&lt;/success&gt;
	&lt;/response&gt;
&lt;/chatterl&gt;</code></pre>

<b>Disonnect from Chatterl</b>
<pre><code>http://CWIGAURL:9000/disconnect/USER</code></pre><p>
Disconnects the client from Chatterl, this destroys the client process along with all the connections it is associated with (needs to be full implemented).</p>

Successful responses are displayed are the following:
<pre><code>{"chatterl":
	{"response":
		{"success":"User dropped"}
	}
}</code></pre>

<pre><code>&lt;chatterl&gt;
	&lt;response&gt;
		&lt;success&gt;User dropped&lt;/success&gt;
	&lt;/response&gt;
&lt;/chatterl&gt;</code></pre>

Where as displayed below:
<pre><code>{"chatterl":
	{"response":
		{"failure":"Unable to drop from group"}
	}
}</code></pre>

<pre><code>&lt;chatterl&gt;
	&lt;response&gt;
		&lt;failure&gt;Unable to drop from group&lt;/failure&gt;
	&lt;/response&gt;
&lt;/chatterl&gt;</code></pre>

<b>List Users on Chatterl</b>
<pre><code>http://CWIGAURL:9000/users/list</code></pre><p>
This call only has two types of response (an empty list or a populated on) which are expressed as follows:</p>

Empty
<pre><code>{"chatterl":
	{"response":
		{"success":
			{"clients":[]}
		}
	}
}</code></pre>

<pre><code>&lt;chatterl&gt;
	&lt;response&gt;
		&lt;success&gt;
			&lt;clients/&gt;
		&lt;/success&gt;
	&lt;/response&gt;
&lt;/chatterl&gt;</code></pre>

Populated
<pre><code>{"chatterl":
	{"response":
		{"success":
			{"clients":[
				{"client":"foo"},
				{"client":"bar"},
				{"client":"baz"}
			]}
		}
	}
}</code></pre>

<pre><code>&lt;chatterl&gt;
	&lt;response&gt;
		&lt;success&gt;
			&lt;clients&gt;
				&lt;client&gt;foo&lt;/client&gt;
				&lt;client&gt;bar&lt;/client&gt;
				&lt;client&gt;baz&lt;/client&gt;
			&lt;/clients&gt;
		&lt;/success&gt;
	&lt;/response&gt;
&lt;/chatterl&gt;</code></pre>

<b>List Users in a Chatterl Group</b>
<pre><code>http://CWIGAURL:9000/users/some_group/list</code></pre><p>
Will list all the clients currently connected to Chatterl, responses to this requests produce the same response body as (<code>/users/list</code>) the above examplexsy.</p>

<b>List Chatterl Groups</b>
<pre><code>http://CWIGAURL:9000/groups/list</code></pre>

<pre><code>{"chatterl":
	{"response":
		{"success":
			{"groups":[
				{"group":"nu_group"},
				{"group":"anuva_group"},
				{"group":"one_more"}
			]}
		}
	}
}</code></pre>

<pre><code>&lt;chatterl&gt;
	&lt;response&gt;
		&lt;success&gt;
			&lt;groups&gt;
				&lt;group&gt;nu_group&lt;/group&gt;
				&lt;group&gt;anuva_group&lt;/group&gt;
				&lt;group&gt;one_more&lt;/group&gt;
			&lt;/groups&gt;
		&lt;/success&gt;
	&lt;/response&gt;
&lt;/chatterl&gt;</code></pre>

<b>Join a Group</b>
<pre><code>http://CWIGAURL:9000/groups/join?client="foo"</code></pre>
Successful requests return the following response:
<pre><code>{"chatterl":
	{"response":
		{"success":"foo joined group"}
	}
}</code></pre>

<pre><code>&lt;chatterl&gt;
	&lt;response&gt;
		&lt;success&gt;foo joined group;/success&gt;
	&lt;/response&gt;
&lt;/chatterl&gt;</code></pre>

Where as failures result in responses simular to the ones below:
<pre><code>{"chatterl":
	{"response":
		{"failure":"foo joined group"}
	}
}</code></pre>

<pre><code>&lt;chatterl&gt;
	&lt;response&gt;
		&lt;failure&gt;foo joined group;/failure&gt;
	&lt;/response&gt;
&lt;/chatterl&gt;</code></pre>

<b>Leave a Groups</b>
<pre><code>http://CWIGAURL:9000/groups/some_group/leave?client="foo"</code></pre><p>
Makes a client leave the specified group, on succes a response simular to below is received.</p>

<pre><code>{"chatterl":
	{"response":
		{"success":"foo has left group foo"}
	}
}</code></pre>

<pre><code>&lt;chatterl&gt;
	&lt;response&gt;
		&lt;success&gt;foo has left group foo;/success&gt;
	&lt;/response&gt;
&lt;/chatterl&gt;</code></pre>

Failure responses are returns as follows:
<pre><code>{"chatterl":
	{"response":
		{"failure":"foo not connected to some_group"}
	}
}</code></pre>

<pre><code>&lt;chatterl&gt;
	&lt;response&gt;
		&lt;failure&gt;foo not connected to some_group;/failure&gt;
	&lt;/response&gt;
&lt;/chatterl&gt;</code></pre>

<b>Send a Group message</b>
<pre><code>http://CWIGAURL:9000/groups/send/some_group?client=foo&amp;msg=hey%20all</code></pre><p>
These responses are dealt with in the same fashion as joining a group.</p>

Successful requests are as follows:
<pre><code>{"chatterl":
	{"response":
		{"success":"msg_sent"}
	}
}</code></pre>

Where as failure responses are returns as follows:
<pre><code>{"chatterl":
	{"response":
		{"failure":"Can not send the same message twice"}
	}
}</code></pre>

<b>Poll Group for messages</b>
<pre><code>http://CWIGAURL:9000/groups/poll/some_group</code></pre>
Checks a groups for a list of its messages, there are two types of successful requests:
<ul>
	<li>No messages</li>
	<li>List of messages</li>
</ul>

No messages are represented as follows:
<pre><code>{"chatterl":{"response":{"messages":[]}}}</code></pre>

<pre><code>&lt;chatterl&gt;
	&lt;response&gt;
		&lt;success&gt;
			&lt;messages/&gt;
		&lt;/success&gt;
	&lt;/response&gt;
&lt;/chatterl&gt;</code></pre>

Lists of messages are expressed in the following manner:
<pre><code>{"chatterl":
	{"response":
		{"success":
			{"messages":
				[{"message":
					[
						{"client":"baph"},
						{"date":ISODATE},
						{"msgbody":"hey"}
					]
				},
				{"message":
					[
						{"client":"baph"},
						{"date":ISODATE},
						{"msgbody":"welcome"}]
				}]
			}
		}
	}
}</code></pre>

<pre><code>&lt;chatterl&gt;
	&lt;response&gt;
		&lt;messages&gt;
			&lt;message&gt;
				&lt;client&gt;baph&lt;/client&gt;
				&lt;date&gt;ISODATE&lt;/date&gt;
				&lt;msgbody&gt;hey&lt;/msgbody&gt;	
			&lt;/message&gt;
			&lt;message&gt;
				&lt;client&gt;baph&lt;/client&gt;
				&lt;date&gt;ISODATE&lt;/date&gt;
				&lt;mgsbody&gt;welcome&lt;/msgbody&gt;	
			&lt;/message&gt;
		&lt;/messages&gt;
	&lt;/response&gt;
&lt;/chatterl&gt;</code></pre>

It is possible for a group not to exist, in these cases the following response will be returned:
<pre><code>{"chatterl":{"response":{"failure":"Group:some_group does'n exist"}}}</code></pre>

<pre><code>&lt;chatterl&gt;
	&lt;response&gt;
		&lt;failure&gt;Group:some_group does'n exist;/failure&gt;
	&lt;/response&gt;
&lt;/chatterl&gt;</code></pre>