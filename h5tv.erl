-module(h5tv).

-export([start/0, stop/0, init/0]).

start() ->
    Pid = spawn(?MODULE, init, []),
    register(?MODULE, Pid).

stop() ->
    ?MODULE ! stop.

init() ->
    h5tv_tcp_listener:start_link(
      8000,
      [binary, {active, false}, {packet, http_bin}, {reuseaddr, true}],
      {h5tv_storage_connection, start, []}),

    h5tv_tcp_listener:start_link(
      8001,
      [binary, {active, false}, {packet, http_bin}, {reuseaddr, true}],
      {h5tv_live_connection, start, []}),

    h5tv_channel_manager:start_link(),

    receive
        stop ->
            exit(shutdown)
    end.
