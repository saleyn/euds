Erlang Unix Domain Socket (euds)
================================

This project implements a NIF library to support Unix Domain Sockets.

The implementation uses two C functions (`do_bind`, `do_connect`) to setup
a socket, and then Erlang implementation assigns an open file descriptor
to either `gen_tcp` or `gen_udp` Erlang socket. This allows to reuse
existing Erlang send/receive API on file descriptors set up externally.

Additionally this NIF library has functions `send_fd/2` and `recv_fd/1` do
send and receive file descriptors through a Unix Domain Socket.

Note: there is a bug in the OTP socket management which requires the Erlang
distribution to be patched in order for this project to work. The patch can
be found here: https://github.com/saleyn/otp/compare/uds.  It was submitted
in the form of a pull request to the Erlang/OTP team:
https://github.com/erlang/otp/pull/612.

See [these instructions](http://www.erlang.org/doc/installation_guide/INSTALL.html#Advanced-configuration-and-build-of-ErlangOTP_Building_How-to-Build-a-Debug-Enabled-Erlang-RunTime-System)
if you need to debug the Erlang's `inet_drv.c` network driver.

### Important ###
Once the above-stated patch is applied, rebuild the OTP distribution, and
check that `erts/preloaded/src/*.erl` have been compiled into
`erts/preloaded/ebin/*.beam` (note that the `Makefile` in that directory
tends to place compiled files in the same directory where the sources are
instead of placing them in `ebin/` (see this closed issue that illustrates
this solution: https://github.com/saleyn/euds/issues/1).

## Author ##

Serge Aleynikov &lt;saleyn at gmail dot com&gt;

## Installation: ##

1. Apply the following patch to the latest Erlang release:
   https://github.com/saleyn/otp/compare/uds.patch
2. Ensure you have a local installation of `rebar`.
3. git clone https://github.com/saleyn/euds.git
4. make

## Usage: ##

### TCP example ###

```erlang
% TCP Unix Domain Socket Server:
1> file:delete("/tmp/test.sock").
ok
2> {ok, S} = gen_uds:listen("/tmp/test.sock", [stream]).
{ok,#Port<0.980>}
3> {ok, CS} = gen_tcp:accept(S).
{ok,#Port<0.981>}
4> inet:setopts(CS, [{active, false}]).
ok
5> gen_tcp:recv(CS, 0).
{ok,"abc"}
6> gen_tcp:close(CS).
ok
7> gen_tcp:close(S).
ok

% TCP Unix Domain Socket Client:
1> {ok, S} = gen_uds:connect("/tmp/test.sock", [stream]).
{ok,#Port<0.949>}
2> gen_tcp:send(S, "abc").
ok
3> gen_tcp:close(S).
ok
```

### UDP example ###

```erlang
% UDP Unix Domain Socket Server:
1> file:delete("/tmp/test.sock").
ok
2> {ok, S} = gen_uds:listen("/tmp/test.sock", [dgram]).
{ok,#Port<0.980>}
3> inet:setopts(S, [{active, once}]).
4> receive Msg -> Msg end,
{udp,#Port<0.980>,"/tmp/test.sock",0,"abc"}
5> inet:setopts(S, [{active, false}]).
ok
6> gen_udp:recv(S, 0).
{ok,{"/tmp/test.sock", 0, "efg"}}
7> gen_udp:close(S).
ok

% UDP Unix Domain Socket Client:
1> {ok, S} = gen_uds:connect("/tmp/test.sock", [dgram]).
{ok,#Port<0.949>}
2> gen_udp:send(S, "abc").
ok
3> gen_udp:send(S, "efg").
ok
4> gen_udp:close(S).
ok
```
