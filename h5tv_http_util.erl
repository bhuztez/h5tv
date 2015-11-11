-module(h5tv_http_util).

-export(
   [ read_http_headers/1,
     http_response/4]).


read_http_headers(Conn) ->
    receive
        continue ->
            ok
    after 5000 ->
            throw(timeout)
    end,

    {ok, {http_request, Method, Path, Version}} = gen_tcp:recv(Conn, 0),
    Headers = recv_headers(Conn),
    ok = inet:setopts(Conn, [{packet, raw}]),
    {Method, Path, Version, Headers}.


recv_headers(Conn) ->
    case gen_tcp:recv(Conn, 0) of
        {ok, {http_header, _, Field, _, Value}} ->
            [{Field, Value}|recv_headers(Conn)];
        {ok, http_eoh} ->
            []
    end.


http_response(Conn, Code, ContentType, Body) ->
    ok =
        gen_tcp:send(
          Conn,
          [<<"HTTP/1.1 ">>, integer_to_list(Code), <<" ">>, httpd_util:reason_phrase(Code), <<"\r\n">>,
           <<"Connection: close\r\n">>,
           <<"Access-Control-Allow-Origin: *\r\n">>,
           <<"Content-Type: ">>, ContentType, <<"\r\n">>,
           <<"Content-Length: ">>, integer_to_list(iolist_size(Body)),
           <<"\r\n\r\n">>, Body]),
    ok = gen_tcp:close(Conn).
