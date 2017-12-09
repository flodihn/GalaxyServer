-module(galaxy_sim).
-behaviour(gen_server).

-define(SIMULATION_STEP_TIME, 2000).
-define(DBMOD, mnesia_galaxy).

-include("galaxy_defs.hrl").

-record(state, {galaxy_id, sim_proc_name}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/1,
    simulate_system/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% gen_server API Function Definitions
%% ------------------------------------------------------------------

start_link(GalaxyId) when is_binary(GalaxyId) ->
    SimulationProcName = get_galaxy_name(GalaxyId),
    case whereis(SimulationProcName) of
        undefined ->
            State = #state{galaxy_id = GalaxyId,
                sim_proc_name = SimulationProcName},
            gen_server:start_link({local, SimulationProcName},
                ?MODULE, [State], []),
            gen_server:cast(SimulationProcName, simulate_systems);
        ExistingPid ->
            {error, {already_running, ExistingPid}}
    end.

%% ------------------------------------------------------------------
%% Galaxy Simulation API Function Definitions
%% ------------------------------------------------------------------

simulate_system(#system{galaxy_id = GalaxyId} = System) ->
    SimulationProcName = get_galaxy_name(GalaxyId),
    gen_server:call(SimulationProcName, {simulate_system, System}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([State]) ->
    {ok, State}.

handle_call({simulate_system, System}, _From, State) ->
    Result = apply(galaxy_system_sim, start_link, [System]),
    {reply, Result, State};

handle_call(Request, _From, State) ->
    error_logger:info_report({unknown_request, Request}),
    {reply, ok, State}.

handle_cast(simulate_systems, #state{galaxy_id = GalaxyId} = State) ->
    {ok, SystemList} = galaxy_srv:get_systems(GalaxyId),
    [apply(galaxy_system_sim, start_link, [System]) || 
        System <- SystemList],
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, GalaxyId, _Extra) ->
    {ok, GalaxyId}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

get_galaxy_name(GalaxyId) ->
    list_to_atom(binary_to_list(GalaxyId) ++ "_galaxy_sim").
