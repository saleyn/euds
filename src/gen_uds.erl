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
-export([init/0, connect/2, listen/2, close/1]).
-export([accept/1, accept/2, async_accept/1, set_sockopt/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type socket()         :: gen_tcp:socket().
-type uds_option()     :: {type, stream | dgram} |
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
    case do_connect(Filename, Options) of
    {ok, FD} when is_integer(FD) ->
        case gen_tcp:fdopen(FD, [local]) of
        {ok,    Sock} -> ok;
        {error, Sock} -> throw({error, inet:format_error(Sock)})
        end,

        inet_db:register_socket(Sock, inet_tcp),

        case prim_inet:setopts(Sock, proplists:delete(type, Options)) of
        ok         -> {ok, Sock};
        {error, E} -> {error, io_lib:format("setopts failed: ~s", [E])}
        end;
    Error ->
        Error
    end.

%% @doc Sets up a UNIX Domain socket to listen on the `Filename' on the local host.
%%      See `gen_tcp:listen/2' and `inet:setopts/2' for a list of available options.
-spec listen(string(), listen_options()) -> {ok, gen_uds:socket()} | {error, any()}.
listen(Filename, Options) when is_list(Filename), is_list(Options) ->
    case do_bind(Filename, Options) of
    {ok, FD} ->
        case type(Options) of
        {ok, M, F, Opts} when M =:= gen_tcp; M =:= gen_udp ->
            case M:F(0, [{fd, FD}, local | Opts]) of
            {ok,    LSock} -> {ok,    LSock};
            {error, LSock} -> do_close(FD), {error, {fdopen, LSock}}
            end;
        Error ->
            do_close(FD),
            throw(Error)
        end;
    Error ->
        Error
    end.

type(Options) ->
    case proplists:split(Options, [type]) of
    {[[{type, stream}]], Opts} ->
        {ok, gen_tcp, listen, Opts};
    {[[{type, dgram}]], Opts} ->
        {ok, gen_udp, open, Opts};
    {[[{type, Type}]], _Opts} ->
        {error, {invalid_type, Type}};
    {[[]], Opts} ->
        {ok, gen_tcp, listen, Opts}
    end.

%% @doc Close a UDS socket.
-spec close(gen_uds:socket()) -> ok.
close(Sock) ->
    gen_tcp:close(Sock).

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
set_sockopt(ListSock, CliSocket) ->
    true = inet_db:register_socket(CliSocket, inet_tcp),
    case prim_inet:getopts(ListSock, [active, nodelay, keepalive, delay_send]) of
    {ok, Opts} ->
        case prim_inet:setopts(CliSocket, Opts) of
        ok    -> ok;
        Error -> gen_tcp:close(CliSocket), Error
        end;
    Error ->
        gen_tcp:close(CliSocket), Error
    end.

%%%----------------------------------------------------------------------------
%%% Internal functions implemented in C
%%%----------------------------------------------------------------------------
-spec do_connect(string(), uds_options()) -> {ok, integer()} | {error, any()}.
do_connect(Filename, Options) when is_list(Filename), is_list(Options) ->
    erlang:nif_error(not_implemented).

-spec do_bind(string(), uds_options()) -> {ok, integer()} | {error, any()}.
do_bind(Filename, Options) when is_list(Filename), is_list(Options) ->
    erlang:nif_error(not_implemented).

do_close(FD) when is_integer(FD) ->
    erlang:nif_error(not_implemented).
    
%%%----------------------------------------------------------------------------
%%% Test functions
%%%----------------------------------------------------------------------------

-ifdef(TEST).
gen_uds_test() ->
    File = "/tmp/test_euds.sock",
    Pid  = self(),
    CPid = spawn_link(fun() ->
        receive ready -> ok
        after   10000 -> exit(timeout)
        end,
        {ok, S} = gen_uds:connect(File, [{type, stream}]),
        gen_tcp:send(S, <<"abc">>),
        ?assertEqual(ok, gen_uds:close(S)),
        Pid ! {client, ok}
    end),
    spawn_link(fun() ->
        file:delete(File),
        {ok, S}  = gen_uds:listen(File, [{type, stream}]),
        CPid ! ready,
        {ok, CS} = gen_uds:accept(S),
        inet:setopts(CS, [{active, false}]),
        {ok, "abc"} = gen_tcp:recv(CS, 0),
        ?assertEqual(ok, gen_uds:close(CS)),
        ?assertEqual(ok, gen_uds:close(S)),
        Pid  ! {server, ok}
    end),

    receive {server, ok} -> ok
    after   5000         -> throw(no_response_from_server)
    end,

    receive {client, ok} -> ok
    after   5000         -> throw(no_response_from_client)
    end.

-endif.
 
