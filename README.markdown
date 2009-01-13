<h1>The chatterl application</h1>
<p>Copyright &copy; 2008 Yomi Colledge</p>
<p><b>Version:</b> Jan 13 2009 03:34:49</p>

<ul>
	<li><a href="#Description">Description</a></li>
	<li><a href="#Installing_Chatterl">Installing Chatterl</a></li>
	<li><a href="#Running_Chatterl">Running Chatterl</a></li>
	<li><a href="#Features">Features</a></li>
	<li><a href="#Future_Features">Future Features</a></li>
	<li><a href="#Useage">Useage</a></li>
	
</ul>

<h3><a name="Description">Description</a></h3><p>
A multi processed chat system that can be housed over a number of nodes and track clients over varying devices, at the time of the writing the system works over multiple nodes and is able to do the basics (connect to a group, send message to other clients and connected groups).</p>

<p>The main focus of this project is to create a chat system that is highly reliable as well as scaleable. Other developers will be able to create add-on modules that are able to interact with chatterl and further enhance the functionality of chatterl and the chatterl clients experience.</p>

<p>As mentioned the system is OTP based, of which it uses Sinan to maintain its builds.</p>

<h3><a name="Installing_Chatterl">Installing Chatterl</a></h3>
At the moment of this writing Chatterl is still in alpha so their is no real release at the moment, to get it running you will need to do the following:
<pre><code>git-clone git://github.com/baphled/chatterl.git &amp;&amp;
cd chatterl &amp;&amp;
sinan doc &amp;&amp; 
sinan dist &amp;&amp;
cd _/build/development/tar &amp;&amp;
sudo faxien install-release chatterl-0.1.1.0.tar.gz</code></pre>

<p>The above presumes that you have Sinan configured &amp; installed, if you haven't refer to erlware.
You will need to change to cookie &amp; name values to something else, doing so should drop you into the erlang shell &amp; ready to run the Chatterl application. </p>

<h3><a name="Running_Chatterl">Running Chatterl</a></h3>
To run a client on a different machine you will need to do the above if  (making sure that the -sname is not the same as any other connected nodes &amp; that the cookie is the same) if connecting on the same box simply cd to the ebin directory and run the following command:
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
    <li> Send message to a group and other clients.</li>
</ul>

<h3><a name="Future_Features">Future Features</a></h3>
<ul>
    <li> Centralised Error logging and data storage.</li>
    <li> Client customisable routines (able to poll RSS feeds, twitter, FB and the such like).</li>
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
Chatterl server runs as an OTP application and uses a supervisor to manage it (in later versions there will be options to spawn multiple servers, allowing for a more fault tolerant chat system). To start up the server you simply need to run the following command:</p>


<p>Which will initialise the server and CWIGA The backend allowing clients to connect and groups to be created and admin the ability to manage the system. Groups can be created on differing nodes as long as the node can communicate with the chatterl_serv.</p>

<p>CWIGA allows for developers to interact with the API, giving them the ability use the basic CRUD functionalities of Chatterl as well as handle clients along with thier messages and other functionality.</p>

<b>Starting a group</b>
<pre><code>chatterl_serv:create("room","description").</code></pre><p>
Which will spawn a group process which users can connect to.</p>

<p>Chatterl groups can be started on any node that can communicate with the server, this allows the user to create a number of groups on varying nodes, helping with general organisation as well a performance and reliablity.</p>

<b>Connection to chatterl</b>
<pre><code>chatter_client:start(UserName).</code></pre><p>
Creating a connection to the server is done by using the following command.</p>

<p>At the time of this writing chatterl_clients can only spawn a client per node, this will later be changed once the web interface has been fully implemented, possibly to a refactoring the client to a parameterised module.
For the moment node users must follow the basic OTP configurations (same cookie, valid DNS name, etc).</p>

<p>This will initialise a user and connect them to chatterl_serv (must be done before users can join a group or communicate with other chatterl users).</p>

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

<h3><a name="CWIGA">CWIGA</a></h3><p>
CWIGA handles all interaction with Chatterl, though for the moment the basics have only been implemented. It can respond in both XML and JSON (at the moment of  writing only XML is functional).</p>

<ul>
	<li><a href="#XML_Structure">XML Structure</a></li>
	<li><a href="#CWIGA_Calls">CWIGA Calls</a></li>
</ul>

<h3><a name="XML_Structure">XML Structure</a></h3>

CWIGA responds with three types of responses:
<ul>
	<li>Success</li>
	<li>Failure</li>
	<li>Error</li>
</ul>

<p><b>Success</b>
When ever a response has been successfully retrieved from Chatter.</p>

<p><b>Failure</b>
When a Chatterl responds with a failure (user cannot connect), CWIGA in turn responds with failure.</p>

<p><b>Error</b>
If some kind of error CWIGA responds with an error.</p>

CWIGA commands are as follows:
<ul>
	<li>List Users.</li>
	<li>List Groups.</li>
	<li>Connect to Chatterl.</li>
	<li>Disconnect to Chatterl.</li>
	<li>Join a Group.</li>
	<li>Drop from a Group.</li>
</ul>

<b>XML Layout</b>
All CWIGA responses follow the same format for simplicity and ease of use. There are three basic response format that can be retrieved from CWIGA:
<ul>
	<li>Empty</li>
	<li>Populated</li>
	<li>Messages</li>
</ul>

<b>Empty</b>
Empty responses are structured as displayed below:
<pre><code>&lt;chatterl&gt;
	&lt;response&gt;
		&lt;users/&gt;
	&lt;/response&gt;
&lt;/chatterl&gt;</code></pre>

<b>Populated</b>
Populated lists are have the following structure:
When CWIGA has a number of result it creates a structure simular to below:
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

<b>Messages</b>
There are a couple of reasons for receiving messages, the first two being used for success &amp; failures (which are used for simple interactions with CWIGA). The other time is when CWIGA has come across an error, messages are formatted in the following format:
<pre><code>
&lt;chatterl&gt;
	&lt;message&gt;
		&lt;failure&gt;Unable to connect.&lt;/failure&gt;
	&lt;/message&gt;
&lt;/chatterl&gt;
</code></pre>

<h3><a name="CWIGA_Calls">CWIGA Calls</a></h3><p>
All commands apart from connect require the client to connect, without it, they will receive an erorr (at the moment this is not true).</p>

<b>Connect</b>
<pre><code>http://CWIGAURL:9000/connect/USER</code></pre><p>
This will give the client a list of users connected to Chatterl.</p>

<b>Disonnect</b>
<pre><code>http://CWIGAURL:9000/disconnect/USER</code></pre><p>
This will give the client a list of users connected to Chatterl.</p>

<b>List Users</b>
<pre><code>http://CWIGAURL:9000/users/list</code></pre>

<b>List Groups</b>
<pre><code>http://CWIGAURL:9000/groups/list</code></pre>

<b>Join Groups</b>
<pre><code>http://CWIGAURL:9000/groups/join?client="foo"</code></pre>

<b>Drop Groups</b>
<pre><code>http://CWIGAURL:9000/groups/drop?client="foo"</code></pre>
Eventually this method will become:
<pre><code>http://CWIGAURL:9000/groups/drop</code></pre>