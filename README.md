erl_tools
=========

Misc Erlang Tools

Sequences / Iteratores:

* fold:       folds
* pfold:      partial folds
* sink:       push-style data consumer callbacks
* igen:       impure generators
* iseq:       infinite sequences implemented as thunks
* source:     pure, idempotent sequences


Core abstractions and algorihtms

* obj:        processes with Map state
* type_base:  serializable data types
* tools:      grab bag of function
* diff:       compute datastructure diffs
* run:        running scripts


Distributed systems:

* epid:       multi-hop pids for fine grained objects / processes
* throttle:   data throttling


TCP

* serv:         simple server tools
* serv_tcp:     simple tcp server with connection list
* http:         gen_tcp wrappers
* cowboy_wrap:  wrappers for cowboy web server

Networking tools

* reverse_tunnel: e.g. for reverse SSH (not what you think)
* socks_proxy:    socks5 proxy server
* bert_rpc:       BERT RPC support
* ssh_leaf:       interacti with ssh machine (commands, rsync, log events)

Web

* ws:         erl<->js comm protocol based on WebSockets on top of cowboy
* ws_widget:  actor based web widgets on top of ws
* ws_layout:  layout functions for ws_widget style web widgets.
* exml:       misc exml functions
* serv_ws:    stand-alone websocket server
* stack_ws:   forth-based websocket protocol for serv_ws

Database

* sqlite:          SQLite wrappers
* kvstore:         generic key value store interface
* sqlite_kvstore:  kvstore on top of sqlite wrapper
* sqlite3_ckeys:   composite key store on top of sqlite


Microcontroller development, See also uc_tools/gdb

* rsp:         GDB RSP protocol
* gdbstub:     gdb stub in Erlang (wip)
* gdbstub_hub: server for gdbstubs
* gdb:         gdb MI access
* lab_board:   driver for uc_tools/gdb based lab control boards


Logging

* recorder:   log file recording (also used for a/v recording)
* player:     log file player (also for a/v playback)

Development support

* expect:      Use Erlang parser to create "expect tests"
* hs:          Haskell bindings
* redo:        implementation of Redo build system in Erlang
* reflection:  Code handling Erlang and other language compilation
* ghcid/ghci:  interact with GHCId and GHCI
* emacs:       emacs interfacing

Linux interfacing

Low level drivers

* gpio_poll:      map gpio events to Erlang events (erl/C)
* v4l:            Video For Linux (erl/C)
* ftdi/ftdi_hub:  manage multiple FTDI connections (e.g. uC dev boards)
* rigol:          interface with Rigol oscilloscope
* linux:          misc linux system interfacing




And some doodles that are not worth mentioning.


