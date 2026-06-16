-module(galaxy_srv_sup).

-behaviour(supervisor).

-include("galaxy_defs.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10, 
    MaxSecondsBetweenRestarts = 30, 

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = transient,
    Shutdown = 6000,

    GalaxySrv = {galaxy_srv, {galaxy_srv, start_link, [mnesia_galaxy]},
        Restart, Shutdown, worker, []},

    GalaxySimSup = {galaxy_sim_sup, {galaxy_sim_sup, start_link, []},
        Restart, Shutdown, supervisor, [dynamic]},

    {ok, {SupFlags, [GalaxySrv, GalaxySimSup]}}.
