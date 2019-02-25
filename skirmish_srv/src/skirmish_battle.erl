-module(skirmish_battle).
-behaviour(gen_statem).
-define(SERVER, ?MODULE).

-include("battle_rules.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2]).

%% ------------------------------------------------------------------
%% gen_statem Function Exports
%% ------------------------------------------------------------------
-export([init/1, callback_mode/0, terminate/3]).


%% ------------------------------------------------------------------
%% States Function Exports
%% ------------------------------------------------------------------

-export([
    battle_loading/3,
    battle_in_progress/3,
    battle_ending/3
    ]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(GalaxyId, SkirmishName) ->
    gen_statem:start_link(
      ?MODULE,
      [GalaxyId, SkirmishName],
      []).

%% ------------------------------------------------------------------
%% gen_statem Function Definitions
%% ------------------------------------------------------------------

init([GalaxyId, SkirmishName]) ->
    {ok, battle_loading, []}.

callback_mode() ->
    state_functions.

terminate(_Reason, _StateName, _State) ->
    ok.

%% ------------------------------------------------------------------
%% States
%% ------------------------------------------------------------------
battle_loading(cast, _Message, _Data) ->
    {ok, battle_loading, []}.

battle_in_progress(cast, _Message, _Data) ->
    {ok, battle_in_progress, []}.

battle_ending(cast, _Message, _Data) ->
    {ok, battle_ending, []}.
