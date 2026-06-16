-module(socket_client_handler).

-export([start_link/1, init/1]).

start_link(Socket) ->
    proc_lib:start_link(?MODULE, init, [Socket]).

init(Socket) ->
    proc_lib:init_ack({ok, self()}),
    loop(Socket).

loop(Socket) ->
    case gen_tcp:recv(Socket, 2) of
        {ok, <<Size:16/big-unsigned-integer>>} ->
            case gen_tcp:recv(Socket, Size) of
                {ok, Payload} ->
                    handle_payload(Payload, Socket),
                    loop(Socket);
                {error, Reason} ->
                    io:format("socket_client_handler: payload recv error ~p~n", [Reason]),
                    gen_tcp:close(Socket)
            end;
        {error, closed} ->
            io:format("socket_client_handler: client disconnected~n"),
            ok;
        {error, Reason} ->
            io:format("socket_client_handler: header recv error ~p~n", [Reason]),
            gen_tcp:close(Socket)
    end.

handle_payload(Payload, Socket) ->
    try
        Request = binary_to_term(Payload),
        case Request of
            news_request ->
                case holonet_srv:get_recent_news() of
                    {atomic, NewsRecs} ->
                        ResponseNews = lists:map(fun(News) ->
                            case News of
                                {news, _Id, Title, Content, Timestamp} ->
                                    {Title, Content, Timestamp};
                                _ -> News
                            end
                        end, NewsRecs),
                        Response = {news_response, ResponseNews},
                        Encoded = term_to_binary(Response),
                        Size = byte_size(Encoded),
                        gen_tcp:send(Socket, <<Size:16/big-unsigned-integer, Encoded/binary>>);
                    Error ->
                        io:format("holonet_srv error: ~p~n", [Error])
                end;
            _ ->
                io:format("socket_client_handler: unknown request ~p~n", [Request])
        end
    catch
        _:Err ->
            io:format("socket_client_handler: failed to handle payload ~p~n", [Err])
    end,
    ok.
