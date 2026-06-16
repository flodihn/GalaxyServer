-module(socket_client_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/1, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Socket) ->
    supervisor:start_child(?MODULE, [Socket]).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 10,
                 period => 10},
    ChildSpec = #{id => socket_client_handler,
                  start => {socket_client_handler, start_link, []},
                  restart => temporary,
                  shutdown => 5000,
                  type => worker,
                  modules => [socket_client_handler]},
    {ok, {SupFlags, [ChildSpec]}}.
