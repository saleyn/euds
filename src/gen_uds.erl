%%% vim:ts=4:sw=4:et
%%%----------------------------------------------------------------------------
%%% @doc Unix Domain Socket support
%%% @author Serge Aleynikov <saleyn@gmail.com>
%%% @copyright 2015 Serge Aleynikov
%%% @end
%%%----------------------------------------------------------------------------
%%% Created: 2015-01-27
%%%----------------------------------------------------------------------------
-module(gen_uds).
-author('saleyn@gmail.com').

-on_load(init/0).

%% API
-export([init/0, connect/2, connect/3, connect/4, listen/2, close/1,
         send/2, send_fd/2, recv_fd/1]).
-export([accept/1, accept/2, async_accept/1, set_sockopt/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type socket()         :: gen_tcp:socket().
-type uds_option()     :: stream | dgram |
                          gen_tcp:option() | gen_udp:option().
-type uds_options()    :: [uds_option()].
-type listen_options() :: [gen_tcp:listen_option() | gen_udp:listen_option()].

-export_type([socket/0, uds_options/0, listen_options/0]).

%%%----------------------------------------------------------------------------
%%% External API
%%%----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @doc Initialize UDS library
%% @end
%%-----------------------------------------------------------------------------
-spec init() -> term().
init() ->
    Dir = case code:priv_dir(euds) of
          D when is_list(D) -> D;
          {error, bad_name} ->
            Base = filename:dirname(filename:dirname(code:which(?MODULE))),
            filename:join(Base, "priv")
          end,
    Lib = filename:join(Dir,"euds_nifs"),
    erlang:load_nif(Lib, 0).

%% @doc Connects to a server on UDS socket associated with the `Filename' on the local host.
%%      See `gen_tcp:connect/2' and `inet:setopts/2' for a list of available options.
-spec connect(string(), uds_options()) ->
    {ok, gen_tcp:socket()|gen_udp:socket()} | {error, any()}.
connect(Filename, Options) when is_list(Filename), is_list(Options) ->
    {UdsOpts, Opts} = split_opts(Options),
    case do_connect(Filename, UdsOpts) of
    {ok, FD} when is_integer(FD), FD >= 0 ->
        {M,F} = mf(connect, UdsOpts),
        case M:F(FD, [local | Opts]) of
        {ok,    Sock} -> ok;
        {error, Sock} -> throw({error, Sock})
        end,

        inet_db:register_socket(Sock, inet_tcp),

        case prim_inet:setopts(Sock, Opts) of
        ok         -> {ok, Sock};
        {error, E} -> {error, {setopts, E}}
        end;
    Error ->
        Error
    end.

%% @doc Method provided for compatibility with gen_tcp:connect/3
connect(Filename, _Port, Options) ->
    connect(Filename, Options).

%% @doc Method provided for compatibility with gen_tcp:connect/4
connect(Filename, _Port, Options, _Timeout) ->
    connect(Filename, Options).
    
%% @doc Sets up a UNIX Domain socket to listen on the `Filename' on the local host.
%%      See `gen_tcp:listen/2' and `inet:setopts/2' for a list of available options.
-spec listen(string(), listen_options()) -> {ok, gen_uds:socket()} | {error, any()}.
listen(Filename, Options) when is_list(Filename), is_list(Options) ->
    {UdsOpts, Opts} = split_opts(Options),
    case do_bind(Filename, UdsOpts) of
    {ok, FD} when is_integer(FD), FD >= 0 ->
        {M,F} = mf(listen, UdsOpts),
        case M:F(0, [{fd, FD}, local | Opts]) of
        {ok,    LSock} -> {ok, LSock};
        {error, LSock} -> do_close(FD), {error, {fdopen, LSock}}
        end;
    Error ->
        Error
    end.

%% @doc Send binary data to a UDS socket.
-spec send(gen_uds:socket(), binary()) -> ok.
send(Sock, Data) when is_port(Sock), is_binary(Data) ->
    {ok, FD} = inet:getfd(Sock),
    do_send(FD, Data).

%% @doc Send a file descriptor to a UDS socket.
-spec send_fd(gen_uds:socket(), binary()) -> ok | {error, any()}.
send_fd(Sock, SendFD) when is_port(Sock), is_integer(SendFD) ->
    {ok, FD} = inet:getfd(Sock),
    do_send_fd(FD, SendFD).

%% @doc Send a file descriptor to a UDS socket.
-spec recv_fd(gen_uds:socket()) -> {ok, integer()} | timeout | {error, any()}.
recv_fd(Sock) when is_port(Sock) ->
    {ok, FD} = inet:getfd(Sock),
    do_recv_fd(FD).

%% @doc Close a UDS socket.
-spec close(gen_uds:socket()) -> ok.
close(Sock) when is_port(Sock) ->
    case inet:getfd(Sock) of
    {ok, FD} -> close(FD);
    _        -> ok
    end,
    gen_tcp:close(Sock);
close(FD)   when is_integer(FD) ->
    do_close(FD).

%% @doc Accepts an incoming connection request on a listen socket. Socket must be a
%%      socket returned from `listen/3'.  This function blocks indefinitely until a
%%      client connection is established.
-spec accept(socket()) -> {ok, socket()} | {error, any()}.
accept(LSock) ->
    gen_tcp:accept(LSock).

%% @doc Accepts an incoming connection request on a listen socket. Socket must be a
%%      socket returned from `listen/3'. Timeout specifies a timeout value in ms.
-spec accept(socket(), integer() | infinity) -> {ok, socket()} | {error, any()}.
accept(LSock, Timeout) ->
    gen_tcp:accept(LSock, Timeout).

%% @doc An asynchronous version of `accept/1'.  The caller will receive
%%      `{inet_async, LSock, Ref::integer(), {ok, CliSocket::socket()}}' or
%%      `{inet_async, ListSock, Ref, {error, Reason}}' upon successful/unsuccessful
%%      client connection.  The function needs to be called again in order to
%%      begin asynchronous accept of the next client socket.  When a successful
%%      socket message `{ok, CliSocket::socket()}' is received, the server must
%%      call `set_sockopt/2' function in order to register the socket with the inet
%%      driver.
%% @end
-spec async_accept(socket()) ->
    {ok, Ref::integer()} | {error, any()}.
async_accept(LSock) ->
    prim_inet:async_accept(LSock, -1).

%% @doc Set socket options and register `Sock' with inet driver.
-spec set_sockopt(LSock::socket(), Sock::socket()) -> ok | {error, any()}.
set_sockopt(ListenSock, CliSocket) ->
    true = inet_db:register_socket(CliSocket, inet_tcp),
    case prim_inet:getopts(ListenSock, [active, nodelay, keepalive, delay_send]) of
    {ok, Opts} ->
        case prim_inet:setopts(CliSocket, Opts) of
        ok    -> ok;
        Error -> gen_tcp:close(CliSocket), Error
        end;
    Error ->
        gen_tcp:close(CliSocket), Error
    end.

%%%----------------------------------------------------------------------------
%%% Internal NIF functions implemented in C
%%%----------------------------------------------------------------------------
-spec do_connect(string(), uds_options()) -> {ok, integer()} | {error, any()}.
do_connect(Filename, Options) when is_list(Filename), is_list(Options) ->
    erlang:nif_error(not_implemented).

-spec do_bind(string(), uds_options()) -> {ok, integer()} | {error, any()}.
do_bind(Filename, Options) when is_list(Filename), is_list(Options) ->
    erlang:nif_error(not_implemented).

-spec do_send(integer(), binary()) -> ok | {error, any()}.
do_send(FD, Data) when is_integer(FD), is_binary(Data) ->
    erlang:nif_error(not_implemented).
    
-spec do_close(integer()) -> ok.
do_close(FD) when is_integer(FD) ->
    erlang:nif_error(not_implemented).
    
-spec do_send_fd(integer(), integer()) -> ok | {error, any()}.
do_send_fd(FD, SendFD) when is_integer(FD), is_integer(SendFD) ->
    erlang:nif_error(not_implemented).

-spec do_recv_fd(integer()) -> {ok, FD::integer()} | timeout | {error, any()}.
do_recv_fd(FD) when is_integer(FD) ->
    erlang:nif_error(not_implemented).

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
split_opts(Options) ->
    lists:partition(fun
        (stream)    -> true;
        (dgram)     -> true;
        (reuseaddr) -> true;
        ({reuseaddr,V}) when is_boolean(V) -> true;
        (_)         -> false
    end, Options).

mf(Action, Options) ->
    case {lists:member(dgram, Options), Action} of
    {true,   listen} -> {gen_udp, open};
    {true,  connect} -> {gen_udp, fdopen};
    {false,  listen} -> {gen_tcp, listen};
    {false, connect} -> {gen_tcp, fdopen}
    end.

%%%----------------------------------------------------------------------------
%%% Test functions
%%%----------------------------------------------------------------------------

-ifdef(TEST).
%-define(assertReceive(Term, Timeout),
%    ?assertMatch(ok, receive Term -> ok after Timeout -> timeout end)).

uds_tcp_test() ->
    File = "/tmp/test_euds.sock",
    Pid  = self(),
    CPid = spawn_link(fun() ->
        ?assertReceive(ready, 1000),
        {ok, S} = gen_uds:connect(File, [stream]),
        gen_tcp:send(S, <<"abc">>),
        timer:sleep(10), % Prevent both packets from being merged in one
        gen_tcp:send(S, <<"efg">>),
        ?assertEqual(ok, gen_tcp:close(S)),
        Pid ! {client, ok}
    end),
    spawn_link(fun() ->
        file:delete(File),
        {ok, S}  = gen_uds:listen(File, [stream]),
        CPid ! ready,
        {ok, CS} = gen_tcp:accept(S),
        inet:setopts(CS, [{active, once}]),
        ?assertReceive({tcp, _Port, "abc"}, 1000),
        inet:setopts(CS, [{active, false}]),
        ?assertEqual({ok, "efg"}, gen_tcp:recv(CS, 0, 1000)),
        ?assertEqual(ok, gen_tcp:close(CS)),
        ?assertEqual(ok, gen_tcp:close(S)),
        Pid  ! {server, ok}
    end),

    ?assertReceive({server, ok}, 2000),
    ?assertReceive({client, ok}, 2000).

uds_udp_test() ->
    File = "/tmp/test_euds.sock",
    Pid  = self(),
    CPid = spawn_link(fun() ->
        ?assertReceive(ready, 1000),
        {ok, S} = gen_uds:connect(File, [dgram]),
        gen_uds:send(S, <<"abc">>),
        gen_uds:send(S, <<"efg">>),
        ?assertEqual(ok, gen_udp:close(S)),
        Pid ! {client, ok}
    end),
    spawn_link(fun() ->
        file:delete(File),
        {ok, S}  = gen_uds:listen(File, [dgram]),
        CPid ! ready,
        % Active socket test
        inet:setopts(S, [{active, once}]),
        ?assertReceive({udp, _Port, File, 0, "abc"}, 1000),
        % Passive socket test
        inet:setopts(S, [{active, false}]),
        ?assertEqual({ok, {File, 0, "efg"}}, gen_udp:recv(S, 0, 1000)),
        ?assertEqual(ok, gen_udp:close(S)),
        Pid  ! {server, ok}
    end),

    ?assertReceive({server, ok}, 2000),
    ?assertReceive({client, ok}, 2000).

-endif.
 
