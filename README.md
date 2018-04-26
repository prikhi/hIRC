# hIRC

`hIRC` is an IRC client that is split into a connection daemon & channel viewer
client. The daemon is responsible for connecting, authenticating, & sending
messages to IRC servers. The client provides a UI for you to view & send
messages to channels.

I desired an IRC client with this architecture so I could always be connected
to all my servers & channels, but view subsets of them via multiple terminals.

For example, I was using `irssi` with `screen` to have a constantly running,
backgroundable IRC client. I had it configured to connect to archlinux, python,
django, haskell, & xmonad channels(and many more). But when I'm working on a
python project, I only care about the python & django channels, everything else
is extra noise and wasted space. Also, there is a single UI that I have to
manipulate when I want to switch from python to haskell channels. This
application lets you eliminate that extra noise & layout work - I can have one
terminal showing my Python channels, and another showing the Haskell channels.
Instead of swapping the Python channels in irssi with Haskell channels, I can
simply switch terminals.


## Develop

You should test against a local IRC server, something like `oragono`:

    sudo packer -S oragono

Then build and run the daemon:

    stack build
    stack exec hircd


## Architecture

### Daemon

Three message queues are used in the daemon. One for sending messages to
Clients via the socket server. One for receiving messages from Clients via the
socket server. And one for areceiving messages from IRC clients.

On startup, initialize the application state, fork connections to the IRC
servers, & fork a socket server.

Then read the message queues and handle any events until the daemon is
terminated.

The IRC forks simply connect to the server, register/identify the user, & adds
any messages to the `IrcQueue`. The IRC server's state is saved in a TVar so
messages can be sent to the server from the main thread.

The socket server simply pipes the `ClientQueue` into the socket & the socket
into the `DaemonQueue`.


## License

GPL-3.0
