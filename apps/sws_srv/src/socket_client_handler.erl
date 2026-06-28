-module(socket_client_handler).

-include("game_packets.hrl").
-include("holonet_srv_defs.hrl").

-export([start_link/1, init/1]).

start_link(Socket) ->
    proc_lib:start_link(?MODULE, init, [Socket]).

init(Socket) ->
    proc_lib:init_ack({ok, self()}),
    %% Ensure never in active mode; always manual gen_tcp recv for our protocol.
    inet:setopts(Socket, [binary, {packet, 0}, {active, false}]),
    loop(Socket).

loop(Socket) ->
    case gen_tcp:recv(Socket, 2) of
        {ok, <<Size:16/big-unsigned-integer>>} ->
            logger:info("socket_client_handler: received header with payload size ~p", [Size]),
            case gen_tcp:recv(Socket, Size) of
                {ok, Payload} ->
                    try
                        handle_payload(Payload, Socket)
                    catch
                        _:Err ->
                            logger:error(
                                "socket_client_handler: failed to handle payload, error ~p, size ~p: ~p",
                                [Err, byte_size(Payload), Payload])
                    end,
                    loop(Socket);
                {error, Reason} ->
                    logger:error("socket_client_handler: payload recv error ~p", [Reason]),
                    gen_tcp:close(Socket)
            end;
        {error, closed} ->
            logger:info("socket_client_handler: client disconnected"),
            ok;
        {error, Reason} ->
            logger:error("socket_client_handler: header recv error ~p", [Reason]),
            gen_tcp:close(Socket)
    end.

handle_payload(<<?CREATE_ACCOUNT_REQUEST, Rest/binary>>, Socket) ->
    case game_protocol:parse_strings(Rest) of
        [Username, Email, Password] ->
            handle_create_account(Username, Email, Password, Socket);
        Strings ->
            logger:warning("socket_client_handler: bad create account payload: ~p", [Strings]),
            game_protocol:send_simple_response(gen_tcp, Socket, ?CREATE_ACCOUNT_RESPONSE, 0)
    end;

handle_payload(<<?DELETE_ACCOUNT_REQUEST, Rest/binary>>, Socket) ->
    case game_protocol:parse_strings(Rest) of
        [Username, Password] ->
            handle_delete_account(Username, Password, Socket);
        Strings ->
            logger:warning("socket_client_handler: bad delete account payload: ~p", [Strings]),
            game_protocol:send_simple_response(gen_tcp, Socket, ?DELETE_ACCOUNT_RESPONSE, 0)
    end;

handle_payload(<<?ACCOUNT_LOGIN_REQUEST, Rest/binary>>, Socket) ->
    case game_protocol:parse_strings(Rest) of
        [Username, Password] ->
            handle_login(Username, Password, Socket);
        Strings ->
            logger:warning("socket_client_handler: bad login payload: ~p", [Strings]),
            game_protocol:send_simple_response(gen_tcp, Socket, ?ACCOUNT_LOGIN_RESPONSE, 0)
    end;

handle_payload(<<?CREATE_CHARACTER_REQUEST, Rest/binary>>, Socket) ->
    Strings = game_protocol:parse_strings(Rest),
    handle_create_character(Strings, Socket);

handle_payload(<<?DELETE_CHARACTER_REQUEST, Rest/binary>>, Socket) ->
    Strings = game_protocol:parse_strings(Rest),
    handle_delete_character(Strings, Socket);

handle_payload(<<?NEWS_REQUEST, _Rest/binary>>, Socket) ->
    handle_news_request(Socket);

handle_payload(Payload, Socket) ->
    logger:warning("socket_client_handler: unknown packet id in payload: ~p", [Payload]),
    game_protocol:send_simple_response(gen_tcp, Socket, 0, 0).

handle_create_account(Username, Email, Password, Socket) ->
    Res = account_srv:create(
        {username, list_to_binary(Username)},
        {email, list_to_binary(Email)},
        list_to_binary(Password)),
    Success = case Res of
        {ok, created} -> true;
        _ -> false
    end,
    game_protocol:send_simple_response(
        gen_tcp, Socket, ?CREATE_ACCOUNT_RESPONSE, if Success -> 1; true -> 0 end).

handle_delete_account(Username, Password, Socket) ->
    Res = account_srv:delete({username, list_to_binary(Username)}, list_to_binary(Password), hard),
    Success = case Res of
        {ok, deleted} -> true;
        _ -> false
    end,
    game_protocol:send_simple_response(
        gen_tcp, Socket, ?DELETE_ACCOUNT_RESPONSE, if Success -> 1; true -> 0 end).

handle_login(Username, Password, Socket) ->
    Res = account_srv:validate({username, list_to_binary(Username)}, list_to_binary(Password)),
    Success = (Res == {ok, validated}),
    game_protocol:send_simple_response(
        gen_tcp, Socket, ?ACCOUNT_LOGIN_RESPONSE, if Success -> 1; true -> 0 end).

handle_create_character(Strings, Socket) ->
    logger:info("stub: create character request: ~p", [Strings]),
    game_protocol:send_simple_response(gen_tcp, Socket, ?CREATE_CHARACTER_RESPONSE, 1).

handle_delete_character(Strings, Socket) ->
    logger:info("stub: delete character request: ~p", [Strings]),
    game_protocol:send_simple_response(gen_tcp, Socket, ?DELETE_CHARACTER_RESPONSE, 1).

handle_news_request(Socket) ->
    case holonet_srv:get_recent_news() of
        {atomic, NewsRecs} ->
            Payload = encode_news_response(NewsRecs),
            game_protocol:send_response(gen_tcp, Socket, Payload);
        Error ->
            logger:error("holonet_srv error: ~p", [Error]),
            game_protocol:send_simple_response(gen_tcp, Socket, ?NEWS_RESPONSE, 0)
    end.

encode_news_response(NewsRecs) ->
    NewsData = lists:foldl(fun encode_news_item/2, <<>>, NewsRecs),
    <<
        ?NEWS_RESPONSE:8,
        (game_protocol:encode_u8(1))/binary,
        (game_protocol:encode_u8(length(NewsRecs)))/binary,
        NewsData/binary
    >>.

encode_news_item(#news{title = Title, content = Content, timestamp = Ts}, Acc) ->
    <<
        Acc/binary,
        (game_protocol:encode_string(Title))/binary,
        (game_protocol:encode_string(Content))/binary,
        (game_protocol:encode_u64(Ts))/binary
    >>.