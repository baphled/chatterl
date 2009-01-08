<h1>Erlang based chat system vsn 0.1.1.0</h1>

At the time of this writing the web interface is based on BeepBeep, which is still in its infancy, as this is the case the builds here are not fully sinan compliant, hopefully we'll have this resolved in the near future.

<h2>Basic Description</h2>
Simple RESTful web interface, allowing others to communicate with Chatterl via a number of mediums.

<h2>Mission Statement</h2>
A chat system that can be housed over a number of nodes and track clients over varying devices, at the time of the writing the system works over multiple nodes and is able to do the basics (connect to a group, send message). 

The main focus of this project is to create a chat system that is highly reliable as well as scaleable. Other developers will be able to create add-on modules that are able to interact with chatterl and further enhance the functionality of chatterl and the chatterl clients experience.

<h2>Installation</h2>
<p>To compile run:
<pre><code>sinan dist</code></pre>

To install:
<pre><code>faxien install-release _build/development/tar/chatterl-0.1.1.0.tar.gz</code></pre>
Which will install Chatterl within your Erlware directory</p>

<h2>Useage</h2>
<b>Starting the web interface</b>
<pre><code>./start-server.sh</code></pre>

<b>Interacting with interface</b>
<pre><code>http://127.0.0.1:8000</code></pre>
