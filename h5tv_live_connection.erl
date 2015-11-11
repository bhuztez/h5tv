-module(h5tv_live_connection).

-export([start/1]).


start(Conn) ->
    handle_request(Conn, h5tv_http_util:read_http_headers(Conn)).

handle_request(Conn, {'GET', {abs_path, <<"/">>}, _Version, _Headers}) ->
    Body =
        [ "[",
          string:join(
            [ io_lib:format("{\"id\": ~p, \"name\": \"~s\"}", [Id, Name])
              || [{Id, Name, _}] <- ets:match(h5tv_channels, '$1')
            ],
            ", "),
          "]"],

    h5tv_http_util:http_response(
      Conn, 200, "application/json", Body);
handle_request(Conn, {'GET', {abs_path, <<"/timestamp">>}, _Version, _Headers}) ->
    h5tv_http_util:http_response(
      Conn,
      200,
      "application/json",
      get_timestamp());
handle_request(Conn, {'GET', {abs_path, <<"/timestamp/", ID/binary>>}, _Version, _Headers}) ->
    [{_,_,Timestamp}] = ets:lookup(h5tv_channels, binary_to_integer(ID)),
    h5tv_http_util:http_response(
      Conn,
      200,
      "application/json",
      Timestamp);
handle_request(Conn, {'GET', {abs_path, <<"/live/", Name/binary>>}, _Version, _Headers}) ->
    timer:send_interval(2500, refresh),
    Timestamp = get_timestamp(),
    ChannelId = gen_server:call(h5tv_channel_manager, {create_channel, Name, Timestamp}),
    gen_tcp:send(
      Conn,
      [<<"HTTP/1.1 200 OK\r\n">>,
       <<"Connection: close\r\n">>,
       <<"Content-Type: text/event-stream\r\n">>,
       <<"Access-Control-Allow-Origin: *\r\n">>,
       <<"Transfer-Encoding: chunked">>,
       <<"\r\n\r\n">>]),
    send_header_path(Conn, ChannelId),
    send_timestamp(Conn, ChannelId, Timestamp),
    loop(Conn, ChannelId);
handle_request(Conn, Request) ->
    io:format("Unknown Request: ~p~n", [Request]),
    h5tv_http_util:http_response(
      Conn, 400, "text/html", <<"<h1>400 Bad Request</h1>">>).


loop(Conn, ChannelId) ->
    receive
        refresh ->
            send_timestamp(Conn, ChannelId)
    end,
    loop(Conn, ChannelId).


send_header_path(Conn, ChannelId) ->
    send_chunked(
      Conn,
      io_lib:format(
        "data: ~p/header.webm\r\n\r\n",
        [ChannelId])).


send_timestamp(Conn, ChannelId) ->
    send_timestamp(Conn, ChannelId, get_timestamp()).


send_timestamp(Conn, ChannelId, Timestamp) ->
    send_chunked(
      Conn,
      io_lib:format(
        "data: ~p/~s.webm\r\n\r\n",
        [ChannelId, Timestamp])).


get_timestamp() ->
    {M, S, _} = os:timestamp(),
    TS = M * 1000000 + S,
    integer_to_list((TS div 5) * 5, 10).


send_chunked(Conn, Data) ->
    gen_tcp:send(
      Conn,
      [integer_to_list(iolist_size(Data), 16),
       "\r\n",
       Data,
       "\r\n"]).
