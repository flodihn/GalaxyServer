-module(galaxy_sim_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-export([start_simulation/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 10, 
    MaxSecondsBetweenRestarts = 30, 

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = transient,
    Shutdown = 6000,

    GalaxySim = {galax_sim, {galaxy_sim, start_link, []}, Restart, 
        Shutdown, worker, []},

    {ok, {SupFlags, [GalaxySim]}}.

 start_simulation(GalaxyId) ->
    supervisor:start_child(?MODULE, [GalaxyId]).
