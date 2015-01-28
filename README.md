Erlang Unix Domain Socket (euds)
================================

This project implements a NIF library to support Unix Domain Sockets.

The implementation uses two C functions (`do_bind`, `do_connect`) to setup
a socket, and then Erlang implementation assigns an open file descriptor
to either `gen_tcp` or `gen_udp` Erlang socket. This allows to reuse
existing Erlang send/receive API on file descriptors set up externally.

Note: there is a bug in the OTP socket management which requires the Erlang
distribution to be patched in order for this project to work. The patch can
be found here: https://github.com/saleyn/otp/compare/uds.  It was submitted
to erlang-patches@erlang.org mailing list for review.

Usage:
======

    % Server:
    1> file:delete("/tmp/test.sock").
    ok
    2> {ok, S} = gen_uds:listen("/tmp/test.sock", [{type, stream}]).
    {ok,#Port<0.980>}
    3> {ok, CS} = gen_uds:accept(S).
    {ok,#Port<0.981>}
    4> inet:setopts(CS, [{active, false}]).
    ok
    5> gen_tcp:recv(CS, 0).
    {ok,"abc"}
    6> gen_tcp:close(CS).
    ok
    7> gen_tcp:close(S).
    ok

    % Client:
    1> {ok, S} = gen_uds:connect("/tmp/test.sock", [{type, stream}]).
    {ok,#Port<0.949>}
    2> gen_tcp:send(S, <<"abc">>).
    ok
    3> gen_tcp:close(S).
    ok

