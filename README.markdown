<h1>Erlang based chat system</h1>

This system is OTP based and uses sinan (erlware) to compile it's sources, see faxien (http://www.erlware.com) for more details.

<h2>Basic Description</h2>
A multi-process chat system, allowing for a single user to connect to multiple groups and send message to other
clients, with the ability to interface with clients over varying mediums.

<h2>Mission Statement</h2>
A chat system that can be housed over a number of nodes and track clients over varying devices, at the time of the writing the system works over multiple nodes and is able to do the basics (connect to a group, send message). 

The main focus of this project is to create a chat system that is highly reliable as well as scaleable. Other developers will be able to create add-on modules that are able to interact with chatterl and further enhance the functionality of chatterl and the chatterl clients experience.

<h2>Features</h2>
<ul>
<li>Client login/logout to chatterl.</li>
<li>View chatterl groups.</li>
<li>View chatterl users.</li>
<li>Login/logout of a chatterl group.</li>
<li>Send message to a group & other clients.</li>
</ul>

<h2>Future Fetures</h2>
<ul>
<li>Centralised Error logging & data storage.</li>
<li>Client customisable routines (able to poll RSS feeds, twitter, FB & the such like).</li>
<li>Better handling of errors.</li>
<li>User registration.</li>
<li>FB Connect.</li>
<li>Chat bots (AIML).</li>
<li>Web interface/API.</li>
<li>Chat modules handler(banning, censorship, chatbots).</li>
</ul>

<h2>Installation</h2>
<p>To compile run:
<pre><code>sinan build</code></pre>
within the root directory of the source file, this will create the _build directory to which the binary files can be located.</p>

<h2>Useage</h2>
<b>Starting the server</b>
Chatterl server runs as an OTP application and uses a supervisor to manage it (in later versions there will be options to spawn multiple servers, allowing for a more fault tolerant chat system). To start up the server you simply need to run the following command:
<pre><code>application:start(chatterl).</code></pre>
Which will initialise the server allowing clients to connect and groups to be created. Groups can be created on differing nodes as long as the node can communicate with the chatterl_serv.

<b>Starting a group</b>
Chatterl groups can be started on any node that can communicate with the server, this allows the user to create a number of groups on varying nodes, helping with general organisation as well a performance and reliablity. 

A group can be initialised by calling the command:
<pre><code>chatterl_serv:create("room","description").</code></pre>
which will create a group process which users can connect to.

<b>Connection to chatterl</b>
At the time of this writing chatterl_clients can only spawn a client per node, this will later be changed once the web interface, possibly to a parameterised module.
For the moment node users must follow the basic OTP configurations (same cookie, valid DNS name, etc). Creating a connection to the server is done by using the following command.
<pre><code>chatter_client:start(UserName).</code></pre>
This will initialise a user and connect them to chatterl_serv (must be done before users can join a group or communicate with other chatterl users).

<b>Disconnecting from chatterl</b>
Chatterl clients can simply disconnect from chatterl by issuing the following command:
<pre><code>chatterl_client:stop().</code></pre>
This will disconnect the user from all the groups they are currently connected to aswell as the actual chatterl server.

<b>Joining a group</b>
This can be done by using the following command:
<pre><code>chatterl_client:join(GroupName).</code></pre>
If the group exists the user is able to join the group allowing them to send message to the room.

<b>Dropping from a group</b>
This is as simple as connection, simply supply the following command:
<pre><code>chatterl_client:drop(GroupName).</code></pre>
This will send a message to the group, which will handle the termination.

<b>Sending group message</b>
<pre><code>chatterl_client:send_msg(GroupName,Message).</code></pre>
GroupName being the name of the group the client is connected to, Message being the message that you want to send to the receiving client. If the message is sent, it will be displayed within the groups node & stored in a messages list which can be polled by joined clients (feature to be added).

<b>Sending a private message</b>
This allows a chatterl client to send a private message to another client, by executing the following:
<pre><code>chatterl_client:private_msg(RecipientName,Message).</code></pre>
If the message is sent successfully then the client will receive follow:
<pre><code>{ok,msg_sent}</pre></code>
and the message will be displayed within the recipients node.
