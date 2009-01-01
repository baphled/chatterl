<h1>Erlang based chat system</h1>

This system is OTP based and uses sinan (erlware) to compile it's sources, see faxien (http://www.erlware.com) for more details.

<h2>Basic Description</h2>
A multi-process chat system, allowing for a single user to connect to multiple groups and send message to other
clients, with the ability to interface with clients over varying mediums.

<h2>Mission Statement</h2>
A chat system that can be housed over a number of nodes and track clients over varying devices, at the time of the writing the system works over multiple nodes and is able to do the basics (connect to a group, send message). 

The main focus of this project is to create a chat system that is highly reliable as well as scaleable. Other developers will be able to create add-on modules that are able to interact with chatterl and further enhance the functionality of chatterl and the chatterl clients experience.

<h2>Features</h2>
Client login/logout to chatterl.
View chatterl groups.
View chatterl users.
Login/logout of a chatterl group.
Send message to a group & other clients.

<h2>Future Fetures</h2>
Centralised Error logging & data storage.
Client customisable routines (able to poll RSS feeds, twitter, FB & the such like).
Better handling of errors.
User registration.
FB Connect.
Chat bots (AIML).
Web interface/API.
Chat modules handler(banning, censorship, chatbots).

<h2>Installation</h2>
<p>To compile run:
<pre><code>sinan build</code></pre>
within the root directory of the source file, this will create the _build directory to which the binary files can be located.</p>

<h2>Useage</h2>
<b>Starting the server</b>
<pre><code>application:start(chatterl).</code></pre>
will initialise the server allowing you to create groups associated to it. Groups can be created accross the node as long as the node can communicate with the server (must use the same cookie value if on different boxes).

<b>Starting a group</b>
Groups can be initialised by calling the command:
<pre><code>chatterl_serv:create("room","description").</code></pre>
which will create a group process which users can connect to.

<b>Connection to chatterl</b>
Node users must follow the basic OTP configurations (same cookie, valid DNS name, etc). Creating a connection to the server is done by using the following command.
<pre><code>chatter_client:start(UserName).</code></pre>
This will initialise a user and connect them to chatterl_serv (this must be done before users can connect to a group or communicate with chatterl users).

<b>Disconnecting from chatterl</b>
Chatterl clients can simply disconnect from chatterl by issuing the following command:
<pre><code>chatterl_client:stop().</code></pre>
This will disconnect the user from all the groups they are currently connected to aswell as the actual chatterl server.

<b>Joining a group</b>
This can be done by using the following command:
<pre><code>chatterl_client:join(GroupName).</code></pre>
If the group exists the user is able to join the group and send message to the room.

<b>Dropping from a group</b>
This is as simple as connection, simply supply the following command:
<pre><code>chatterl_client:drop(GroupName).</code></pre>
This will send a message to the group, which will handle the termination.

<b>Sending group message</b>
<pre><code>chatterl_client:send_msg(GroupName,Message).</code></pre>
GroupName being the name of the group the client is connected to & Message being the message that you want to send to the receiving client.

<b>Sending a private message</b>
This allows a chatterl client to send a private message to another client, by executing the following:
<pre><code>chatterl_client:private_msg(RecipientName,Message).</code></pre>
