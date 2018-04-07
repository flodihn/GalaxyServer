-module(sws_srv_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10, 
    MaxSecondsBetweenRestarts = 30, 

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = transient,
    Shutdown = 6000,

	ElliOpts = [{callback, rest_callback}, {port, 2000}],
	
    SwsSrv = {
        sws_srv,
        {sws_srv, start_link, []},
        Restart,
        Shutdown,
        worker,
        []},
	
	RestAPI = {
        rest_api,
        {elli, start_link, [ElliOpts]},
        permanent,
        5000,
        worker,
        [elli]},

    {ok, {SupFlags, [SwsSrv, RestAPI]}}.

