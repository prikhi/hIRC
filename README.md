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


## License

GPL-3.0
