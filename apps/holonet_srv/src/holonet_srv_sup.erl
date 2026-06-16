-module(holonet_srv_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = permanent,
    Shutdown = 2000,

    Holonet = {
        holonet_srv,
        {holonet_srv, start_link, []},
        Restart,
        Shutdown,
        worker,
        [holonet_srv]},

    {ok, {SupFlags, [Holonet]}}.
