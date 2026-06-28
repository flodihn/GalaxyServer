-module(socket_acceptor_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/1, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Arg) ->
    %% Arg can be ListenSocket (plain) or {ListenSocket, TransportModule} for TLS
    supervisor:start_child(?MODULE, [Arg]).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 10,
                 period => 10},
    ChildSpec = #{id => socket_acceptor,
                  start => {socket_acceptor, start_link, []},
                  restart => temporary,
                  shutdown => 5000,
                  type => worker,
                  modules => [socket_acceptor]},
    {ok, {SupFlags, [ChildSpec]}}.
