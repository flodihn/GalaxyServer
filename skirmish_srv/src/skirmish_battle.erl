-module(skirmish_battle).
-behaviour(gen_statem).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2]).

%% ------------------------------------------------------------------
%% gen_statem Function Exports
%% ------------------------------------------------------------------
-export([init/1, callback_mode/0, terminate/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(GalaxyId, SkirmishName) ->
    gen_statem:start_link(
      ?MODULE,
      [GalaxyId, SkirmishName],
      []).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init([GalaxyId, SkirmishName]) ->
    {ok, initial_state_name, initial_state}.

callback_mode() ->
    state_functions.

terminate(_Reason, _StateName, _State) ->
    ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

