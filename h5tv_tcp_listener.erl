-module(h5tv_tcp_listener).

-export([start_link/3, init/3]).


start_link(Port, Options, Handler) ->
    spawn_link(?MODULE, init, [Port, Options, Handler]).

init(Port, Options, Handler) ->
    {ok, Sock} = gen_tcp:listen(Port, Options),
    loop(Sock, Handler).

loop(Sock, Handler = {M, F, A}) ->
    {ok, Conn} = gen_tcp:accept(Sock),
    Pid = spawn(M, F, [Conn|A]),
    gen_tcp:controlling_process(Conn, Pid),
    Pid ! continue,
    loop(Sock, Handler).
