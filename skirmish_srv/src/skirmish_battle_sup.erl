-module(skirmish_battle_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-export([start_battle/2]).

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

    SkirmishBattle = {
        skirmish_battle,
        {skirmish_battle, start_link, []}, 
        Restart,
        Shutdown,
        worker,
        []},

    {ok, {SupFlags, [SkirmishBattle]}}.

 start_battle(GalaxyId, SkirmishName) ->
    supervisor:start_child(?MODULE, [GalaxyId, SkirmishName]).
