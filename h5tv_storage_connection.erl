-module(h5tv_storage_connection).

-export([start/1]).


start(Conn) ->
    handle_request(Conn, h5tv_http_util:read_http_headers(Conn)).


content_type(<<".html">>) ->
    <<"text/html; charset=utf-8">>;
content_type(<<".js">>) ->
    <<"application/javascript; charset=utf-8">>;
content_type(<<".json">>) ->
    <<"application/json; charset=utf-8">>;
content_type(<<".css">>) ->
    <<"text/css; charset=utf-8">>;
content_type(_) ->
    <<"application/octet-stream">>.

handle_request(Conn, {'GET', {abs_path, <<"/">>}, Version, Headers}) ->
    handle_request(Conn, {'GET', {abs_path, <<"/index.html">>}, Version, Headers});
handle_request(Conn, {'GET', {abs_path, <<"/", Name/binary>>}, _, _Headers}) ->
    [Path|_] = binary:split(Name, <<"?">>),
    case file:read_file(<<"static/", Path/binary>>) of
        {ok, Bin} ->
            h5tv_http_util:http_response(
              Conn,
              200,
              content_type(filename:extension(Path)),
              Bin);
        {error, _} ->
            h5tv_http_util:http_response(
              Conn, 404, "text/html", <<"<h1>404 Not Found</h1>">>)
    end;
handle_request(Conn, {'PUT', {abs_path, <<"/", Name/binary>>}, _, Headers}) ->
    [Name1|_] = binary:split(Name, <<"?">>),
    Path = <<"static/", Name1/binary>>,
    Dirname = filename:dirname(Path),
    case file:make_dir(Dirname) of
        ok ->
            ok;
        {error, eexist} ->
            ok
    end,
    {ok, File} = file:open(Path, [write, binary]),
    Size = binary_to_integer(proplists:get_value('Content-Length', Headers)),
    write_file(File, Conn, Size);
handle_request(Conn, Request) ->
    io:format("Unknown Request: ~p~n", [Request]),
    h5tv_http_util:http_response(
      Conn, 400, "text/html", <<"<h1>400 Bad Request</h1>">>).


write_file(File, Conn, 0) ->
    file:close(File),
    h5tv_http_util:http_response(
      Conn,
      200,
      "text/plain",
      "OK");
write_file(File, Conn, Size) ->
    case gen_tcp:recv(Conn, 0) of
        {ok, Bin} ->
            file:write(File, Bin),
            write_file(File, Conn, Size-byte_size(Bin));
        {error, closed} ->
            file:close(File)
    end.
