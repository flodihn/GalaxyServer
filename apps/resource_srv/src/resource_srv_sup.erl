-module(resource_srv_sup).

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

    ResourceSrv = {
        resource_srv,
        {resource_srv, start_link, [mnesia_resource]},
        Restart,
        Shutdown,
        worker,
        []},

    ResourceEventManager = {
        resource_event_manager,
        {gen_event, start_link, [{local, resource_event_manager}]},
        Restart,
        Shutdown,
        worker,
        []},

    {ok, {SupFlags, [ResourceSrv, ResourceEventManager]}}.

