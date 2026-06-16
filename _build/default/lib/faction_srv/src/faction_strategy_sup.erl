-module(faction_strategy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-export([start_strategy/1]).

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

    FactionStrategySim = {faction_strategy_sim,
		{faction_strategy_sim, start_link, []}, Restart, 
        Shutdown, worker, []},

    {ok, {SupFlags, [FactionStrategySim]}}.

 start_strategy(Faction) ->
    supervisor:start_child(?MODULE, [Faction]).
