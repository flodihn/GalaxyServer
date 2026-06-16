-module(faction_strategy_sim).
-behaviour(gen_server).

-define(SIMULATION_STEP_TIME, 10000).

-include("faction_defs.hrl").

-record(state, {
    galaxy_id,
    faction_name,
	sim_proc_name,
    last_run_time}).

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
%% Internal Function Exports
%% ------------------------------------------------------------------
-export([tick/1]).

%% ------------------------------------------------------------------
%% gen_server API Function Definitions
%% ------------------------------------------------------------------

start_link(#faction{galaxy_id = GalaxyId, name = FactionName}) ->
    SimulationName = get_faction_strategy_sim_name(GalaxyId, FactionName),
    State = #state{
        galaxy_id = GalaxyId, 
        sim_proc_name = SimulationName,
        faction_name = FactionName},
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

handle_call(tick, _From, #state{galaxy_id = GalaxyId, faction_name = FactionName,
		last_run_time = LastRunTime} = State) ->
    Now = get_run_time(),
	DeltaTime = get_delta_time(Now, LastRunTime),
	{ok, Faction} = faction_srv:get_faction(GalaxyId, FactionName),
	simulate_faction(Faction, DeltaTime),
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

get_faction_strategy_sim_name(GalaxyId, FactionName) ->
    list_to_atom(binary_to_list(GalaxyId) ++ "_" ++
    binary_to_list(FactionName) ++ "_strategy_sim").

get_delta_time(Now, Past) ->
    timer:now_diff(Now, Past) / 1000000.

get_run_time() ->
    erlang:timestamp().

simulate_faction(Faction, DeltaTime) ->
	StrategyList = Faction#faction.strategy_modules,
	simulate_strategies(StrategyList, Faction, DeltaTime).

simulate_strategies([], _Faction, _DeltaTime) ->
    done;

simulate_strategies([StrategyModule| Rest], Faction, DeltaTime) ->
	StrategyModule:simulate_faction(Faction, DeltaTime),
	simulate_strategies(Rest, Faction, DeltaTime).

%% ------------------------------------------------------------------
%% Internal Tick process.
%% This operation is potentially harmful since it does not check
%% if the old simulation has finished before ticking.
%% ------------------------------------------------------------------
tick(SimulationProcName) ->
    gen_server:call(SimulationProcName, tick),
    timer:sleep(?SIMULATION_STEP_TIME),
    tick(SimulationProcName).
