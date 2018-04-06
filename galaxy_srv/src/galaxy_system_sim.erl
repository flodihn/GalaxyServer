-module(galaxy_system_sim).
-behaviour(gen_server).

-define(SIMULATION_STEP_TIME, 2000).

-include("galaxy_defs.hrl").

-record(state, {
    galaxy_id,
    sim_callback,
    system_name,
    sim_proc_name,
    last_run_time}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Intenral Function Exports
%% ------------------------------------------------------------------
-export([tick/1]).

%% ------------------------------------------------------------------
%% gen_server API Function Definitions
%% ------------------------------------------------------------------

start_link(#system{galaxy_id = GalaxyId, name = SystemName}, SimCallback) ->
    SimulationName = get_system_name(GalaxyId, SystemName),
    State = #state{
        galaxy_id = GalaxyId, 
        sim_callback = SimCallback,
        sim_proc_name = SimulationName,
        system_name = SystemName},
    gen_server:start_link({local, SimulationName}, ?MODULE, [State], []).

%% ------------------------------------------------------------------
%% Galaxy Simulation API Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([#state{sim_proc_name = SimProcName} = State]) ->
    spawn_link(?MODULE, tick, [SimProcName]),
    Now = get_run_time(),
    {ok, State#state{last_run_time = Now}}.

handle_call(tick, _From, #state{
        sim_callback = SimCallback,
        galaxy_id = GalaxyId,
        system_name = SystemName,
        last_run_time = LastRunTime} = State) ->
    Now = get_run_time(),
    {ok, System} = galaxy_srv:get_system(GalaxyId, SystemName),
    DeltaTime = get_delta_time(Now, LastRunTime),
    SimCallback:simulate_system(System, DeltaTime),
    {reply, ok, State#state{last_run_time=Now}};

handle_call(Request, _From, GalaxyId) ->
    error_logger:info_report({unknown_request, Request}),
    {reply, ok, GalaxyId}.

handle_cast(_Msg, GalaxyId) ->
    {noreply, GalaxyId}.

handle_info(_Info, GalaxyId) ->
    {noreply, GalaxyId}.

terminate(_Reason, _GalaxyId) ->
    ok.

code_change(_OldVsn, GalaxyId, _Extra) ->
    {ok, GalaxyId}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

get_system_name(GalaxyId, SystemName) ->
    list_to_atom(binary_to_list(GalaxyId) ++ "_" ++
    binary_to_list(SystemName) ++ "_system_sim").

get_delta_time(Now, Past) ->
    timer:now_diff(Now, Past) / 1000000.

get_run_time() ->
    erlang:timestamp().

simulate_system(System, DeltaTime) ->
    GalaxyId = System#system.galaxy_id,
    Planets = System#system.planets,
    simulate_planets(Planets, GalaxyId, DeltaTime).

simulate_planets([], _GalaxyId, _DeltaTime) ->
    done;

simulate_planets([PlanetName | Rest], GalaxyId, DeltaTime) ->
    simulate_planet(GalaxyId, PlanetName, DeltaTime),
    simulate_planets(Rest, GalaxyId, DeltaTime).

simulate_planet(GalaxyId, PlanetName, DeltaTime) ->
    {ok, Planet} = galaxy_srv:get_planet(GalaxyId, PlanetName),
    Structures = Planet#planet.structures,
    {ok, UpdatedStructures} = galaxy_structure_util:simulate_structures(
        Structures, DeltaTime),
    UpdatedPlanet = Planet#planet{structures=UpdatedStructures},
    {ok, planet_updated} = galaxy_srv:update_planet(UpdatedPlanet).

%% ------------------------------------------------------------------
%% Internal Tick process. 
%% ------------------------------------------------------------------
tick(SimulationProcName) ->
    gen_server:call(SimulationProcName, tick),
    timer:sleep(?SIMULATION_STEP_TIME),
    tick(SimulationProcName).
