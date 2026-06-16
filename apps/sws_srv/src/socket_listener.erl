-module(socket_listener).

-behaviour(gen_server).

-export([start_link/0, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-ifndef(ACCEPTOR_POOL_SIZE).
-define(ACCEPTOR_POOL_SIZE, 10).
-endif.

start_link() ->
    Port = case application:get_env(sws_srv, socket_port) of
               {ok, P} when is_integer(P) -> P;
               _ -> 9000
           end,
    start_link(Port).

start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

init([Port]) ->
    Opts = [binary,
            {packet, 0},
            {active, false},
            {reuseaddr, true},
            {nodelay, true}],
    case gen_tcp:listen(Port, Opts) of
        {ok, ListenSocket} ->
            io:format("socket_listener: listening on port ~p~n", [Port]),
            PoolSize = case application:get_env(sws_srv, acceptor_pool_size) of
                           {ok, N} when is_integer(N) -> N;
                           _ -> ?ACCEPTOR_POOL_SIZE
                       end,
            %% Start initial pool of PoolSize concurrent acceptors.
            %% Each acceptor will accept and then replenish the pool.
            %% Default comes from -D ACCEPTOR_POOL_SIZE in rebar.config
            start_acceptor_pool(ListenSocket, PoolSize),
            {ok, #{listen_socket => ListenSocket, port => Port}};
        {error, Reason} ->
            {stop, Reason}
    end.

start_acceptor_pool(_ListenSocket, 0) -> ok;
start_acceptor_pool(ListenSocket, N) ->
    socket_acceptor_sup:start_child(ListenSocket),
    start_acceptor_pool(ListenSocket, N-1).

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case maps:get(listen_socket, State, undefined) of
        undefined -> ok;
        LS -> gen_tcp:close(LS)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
