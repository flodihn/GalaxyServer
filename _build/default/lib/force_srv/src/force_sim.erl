-module(force_sim).

%% rewrite to gen_fsm
-behaviour(gen_server).

-define(SIMULATION_STEP_TIME, 10000).

-include("../faction_srv/include/faction_defs.hrl").
-include("force_defs.hrl").

-record(state, {
    galaxy_id,
    force_id,
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
%% gen_server API Function Definitions
%% ------------------------------------------------------------------

start_link(GalaxyId, ForceId) ->
    State = #state{
        galaxy_id = GalaxyId, 
        force_id = ForceId},
    gen_server:start_link(?MODULE, ?MODULE, [State], []).

%% ------------------------------------------------------------------
%% Galaxy Simulation API Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([#state{} = State]) ->
    Now = get_run_time(),
    {ok, State#state{last_run_time = Now}}.

handle_call(tick, _From, #state{galaxy_id = GalaxyId, force_id = ForceId,
		last_run_time = LastRunTime} = State) ->
    Now = get_run_time(),
	DeltaTime = get_delta_time(Now, LastRunTime),
    {reply, ok, State#state{last_run_time=Now}};

handle_call(Request, _From, GalaxyId) ->
    error_logger:info_report({unknown_request, Request}),
    {reply, ok, GalaxyId}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

get_delta_time(Now, Past) ->
    timer:now_diff(Now, Past) / 1000000.

get_run_time() ->
    erlang:timestamp().

%% ------------------------------------------------------------------
%% Internal Tick process.
%% This operation is potentially harmful since it does not check
%% if the old simulation has finished before ticking.
%% ------------------------------------------------------------------
