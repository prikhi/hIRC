# hIRC

[![hIRC Travis-CI Build Status](https://travis-ci.org/prikhi/hIRC.svg?branch=master)](https://travis-ci.org/prikhi/hIRC)

`hIRC` is an IRC client that is split into a connection daemon & channel viewer
client. The daemon is responsible for connecting, authenticating, & sending
messages to IRC servers. The client provides a UI for you to view & send
messages to channels.

## Develop

You should test against a local IRC server, something like `oragono`:

    sudo packer -S oragono

Then build and run the daemon:

    stack build
    stack exec hircd

And run the client in another terminal:

    stack exec hirc


## Motivation

Why not just use `irssi` coupled with `screen`?

I was! But I always desired an IRC client with this specific architecture so I
could always be connected to all my servers & channels, but view subsets of
them via multiple terminals.

With `irssi` & `screen` I had a constantly running, backgroundable IRC client
that would automatically connect to archlinux, python, django, haskell, &
xmonad channels(and many more).

However, when I'm working on a python project I only care about the python &
django channels - everything else is extra noise and wasted space. And irssi
only presents a single UI so I had to do manual layout manipulation when I want
to switch from python to haskell channels.

`hIRC` lets you eliminate that extra noise & layout work - I can have one
terminal showing my Python channels, and another showing the Haskell channels.
Instead of swapping the Python channels with Haskell channels, I can simply
switch terminals.


## Architecture

### Daemon

Three message queues are used in the daemon. One for sending messages to
Clients via the socket server. One for receiving messages from Clients via the
socket server. And one for areceiving messages from IRC clients.

On startup, initialize the application state, fork connections to the IRC
servers, & fork a socket server.

Then it reads the message queues and handle any events until the daemon is
terminated.

The IRC forks simply connect to the server, register/identify the user, & adds
any messages to the `IrcQueue`. The IRC server's state is saved in a TVar so
messages can be sent to the server from the main thread.

The socket server simply pipes the `ClientQueue` into the socket & the socket
into the `DaemonQueue`.


### Client

The client is currently super simple.

On startup, it creates message queues from/to the daemon, connects to the
daemon's socket server, & starts the Brick application.

The UI contains the message log for the current channel, as well as an input
form. When the input form is submitted, the Client will send the message to the
Daemon, which sends it to the IRC server as well as any clients subscribed to
that channel.

When a new IRC message is received from the daemon, it is added to the
respective channel's message log.


### Message Flow

When a client connects to the daemon, the daemon sends it a Hello message
containing it's Client ID & the list of available channels. The client can then
reply with a Subscribe message containing the list of channels it cares about.

Then when a client sends a chat message or the IRC server sends us a new
message, the message is forwarded to any clients subscribed to the channel.

When a client exits, it sends a Goodbye message to let the daemon know it can
close it's message queue & socket connection.


## License

GPL-3.0
