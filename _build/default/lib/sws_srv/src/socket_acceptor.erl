-module(socket_acceptor).

-export([start_link/1, init/1]).

start_link(ListenSocket) ->
    proc_lib:start_link(?MODULE, init, [ListenSocket]).

init(ListenSocket) ->
    proc_lib:init_ack({ok, self()}),
    accept(ListenSocket).

accept(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, ClientSocket} ->
            %% Replenish the acceptor pool by starting a new one
            socket_acceptor_sup:start_child(ListenSocket),
            %% Now this process continues as the client handler
            io:format("socket_acceptor: accepted client, now handling~n"),
            handle_client(ClientSocket);
        {error, closed} ->
            ok;
        {error, Reason} ->
            io:format("socket_acceptor: accept error ~p, will try again~n", [Reason]),
            %% Optionally sleep or just retry by tail calling
            timer:sleep(100),
            accept(ListenSocket)
    end.

handle_client(Socket) ->
    case gen_tcp:recv(Socket, 2) of
        {ok, <<Size:16/big-unsigned-integer>>} ->
            case gen_tcp:recv(Socket, Size) of
                {ok, Payload} ->
                    handle_payload(Payload, Socket),
                    handle_client(Socket);
                {error, Reason} ->
                    io:format("socket_acceptor: payload recv error ~p~n", [Reason]),
                    gen_tcp:close(Socket)
            end;
        {error, closed} ->
            io:format("socket_acceptor: client disconnected~n"),
            ok;
        {error, Reason} ->
            io:format("socket_acceptor: header recv error ~p~n", [Reason]),
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
                io:format("socket_acceptor: unknown request ~p~n", [Request])
        end
    catch
        _:Err ->
            io:format("socket_acceptor: failed to handle payload ~p~n", [Err])
    end,
    ok.
