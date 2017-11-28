-module(galaxy_sim).
-behaviour(gen_server).

-define(SIMULATION_STEP_TIME, 2000).
-define(DBMOD, mnesia_galaxy).

-include("galaxy_defs.hrl").

-record(state, {galaxy_id, last_run_time}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

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

start_link(GalaxyId) when is_binary(GalaxyId) ->
    GalaxyName = get_galaxy_name(GalaxyId),
    gen_server:start_link({local, GalaxyName}, ?MODULE, [GalaxyId], []).

%% ------------------------------------------------------------------
%% Galaxy Simulation API Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([GalaxyId]) ->
    GalaxyName = get_galaxy_name(GalaxyId),
    spawn_link(?MODULE, tick, [GalaxyName]),   
    {ok, #state{galaxy_id=GalaxyId, last_run_time=get_run_time()}}.

handle_call(tick, _From, #state{galaxy_id=GalaxyId,
        last_run_time=LastRunTime} = State) ->
    Now = get_run_time(),
    DeltaTime = get_delta_time(Now, LastRunTime),
    error_logger:info_report({delta_time, DeltaTime}),
    simulate_galaxy(GalaxyId, DeltaTime),
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

get_galaxy_name(GalaxyId) ->
    list_to_atom(binary_to_list(GalaxyId) ++ "_galaxy").

get_delta_time(Now, Past) ->
    timer:now_diff(Now, Past) / 1000000.

get_run_time() ->
    erlang:timestamp().

simulate_galaxy(GalaxyId, DeltaTime) ->
    {ok, AllSystems} = galaxy_srv:get_systems(GalaxyId),
    simulate_systems(AllSystems, DeltaTime).

simulate_systems([], _DeltaTime) ->
    done;

simulate_systems([System| Rest], DeltaTime) ->
    simulate_system(System, DeltaTime),
    simulate_systems(Rest, DeltaTime).

simulate_system(System, DeltaTime) ->
    GalaxyId = System#system.galaxy_id,
    [simulate_planet(GalaxyId, PlanetName, DeltaTime) || PlanetName <- 
        System#system.planets].

simulate_planet(GalaxyId, PlanetName, DeltaTime) ->
    {ok, Planet} = galaxy_srv:get_planet(GalaxyId, PlanetName),
    UpdatedStructures = [galaxy_structure_util:simulate_structure(
        Structure, DeltaTime) || Structure <- Planet#planet.structures],
    UpdatedPlanet = Planet#planet{structures=UpdatedStructures},
    galaxy_srv:update_planet(UpdatedPlanet).

%% ------------------------------------------------------------------
%% Internal Tick process. 
%% ------------------------------------------------------------------
tick(GalaxyName) ->
    gen_server:call(GalaxyName, tick),
    timer:sleep(?SIMULATION_STEP_TIME),
    tick(GalaxyName).

hourly_resource_rate(Amount, DeltaTime) ->
    1/3600 * Amount * DeltaTime.

