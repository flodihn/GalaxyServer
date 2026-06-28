-module(sws_srv_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Start function from shell (only for development)
%% ===================================================================
start() ->
    sws_srv_logger:configure(),
    mnesia:start(),
    ssl:start(),  % for optional TLS support in socket_listener_init
    application:start(holonet_srv),
    application:start(account_srv),
    %application:start(resource_srv),
    %application:start(galaxy_srv),
   % application:start(faction_srv),
   % application:start(battle_srv),
   % application:start(economy_srv),
    application:start(sws_srv),
    maybe_start_observer().
    %galaxy_srv:start_simulation(),
    %faction_srv:simulate_strategies().

maybe_start_observer() ->
    case application:get_env(sws_srv, start_observer) of
        {ok, true} -> observer:start();
        _ -> ok
    end.

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    sws_srv_logger:configure(),
    sws_srv_sup:start_link().

stop(_State) ->
    ok.
