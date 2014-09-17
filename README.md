ghc-mod-socket
==============

Makes `ghc-modi` behave like hdevtools.

Note that this is only a quick proof of concept so far, but it works.

## Running

Start `ghc-mod-socket` from your cabal directory or below.

You can then communicate with the socket file, e.g.:

`echo "check myfile.hs" | nc -U .ghc-mod-socket.sock`

Binding this to a command in your editor might be useful.
