-module(skirmish_srv_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(skirmish_srv),
    observer:start().

start(_StartType, _StartArgs) ->
    skirmish_battle_sup:start_link().

stop(_State) ->
    ok.
