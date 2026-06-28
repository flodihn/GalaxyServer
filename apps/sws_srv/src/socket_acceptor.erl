-module(socket_acceptor).

-behaviour(gen_statem).

-include("socket_acceptor.hrl").

-export([start_link/1]).
-export([callback_mode/0, init/1, terminate/3]).
-export([recv_packet/2, packet_id/1]).

-export([
    accepting/3,
    connected/3,
    playing/3
]).

start_link(Arg) ->
    gen_statem:start_link(?MODULE, [Arg], []).

callback_mode() ->
    state_functions.

init([Arg]) ->
    {ListenSocket, Transport} = parse_init_arg(Arg),
    sws_srv_logger:info(
        "socket_acceptor: started ~p (transport=~p)",
        [self(), Transport]),
    {ok, accepting, #data{listen_socket = ListenSocket, transport = Transport},
     [{next_event, internal, accept}]}.

terminate(_Reason, _State, #data{client_socket = Socket, transport = Transport}) ->
    case Socket of
        undefined -> ok;
        _ -> Transport:close(Socket)
    end.

accepting(EventType, Event, Data) ->
    accepting:handle(EventType, Event, Data).

connected(EventType, Event, Data) ->
    connected:handle(EventType, Event, Data).

playing(EventType, Event, Data) ->
    playing:handle(EventType, Event, Data).

parse_init_arg({ListenSocket, Transport}) ->
    {ListenSocket, Transport};
parse_init_arg(ListenSocket) ->
    {ListenSocket, gen_tcp}.

recv_packet(Socket, Transport) ->
    case Transport:recv(Socket, 2) of
        {ok, <<Size:16/big-unsigned-integer>>} ->
            case Transport:recv(Socket, Size) of
                {ok, Payload} ->
                    logger:info("socket_acceptor: received packet id=~p bytes=~p",
                                [packet_id(Payload), Size]),
                    {ok, Payload};
                {error, closed} ->
                    closed;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, closed} ->
            closed;
        {error, Reason} ->
            {error, Reason}
    end.

packet_id(<<PacketId:8, _/binary>>) ->
    PacketId;
packet_id(_) ->
    unknown.