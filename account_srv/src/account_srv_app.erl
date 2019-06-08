-module(account_srv_app).
-behaviour(application).

-export([
    start/0,
    start/2,
    stop/1
    ]).

start() ->
    application:start(account_srv).

start(_Type, StartArgs) ->
    account_srv_sup:start_link(StartArgs).

stop(_State) ->
    ok.
