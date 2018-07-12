-module(sws_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
		create_squadron/3,
		upgrade_squadron_to_wing/1,
		upgrade_wing_to_fleet/1,
		upgrade_code/1,
		disband/1,
		assemble_force/2,
		start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([
		 init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
         terminate/2,
		 code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

create_squadron(SquadronName, Founder, Members) ->
	gen_server:call(?SERVER, {create_squadron, SquadronName, Founder,
		Members}).

upgrade_squadron_to_wing(SquadronId) ->
	gen_server:call(?SERVER, {upgrade_squadron_to_wing, SquadronId}).

upgrade_wing_to_fleet(WingId) ->
	gen_server:call(?SERVER, {upgrade_wing_to_fleet, WingId}).

disband(Id) ->
	gen_server:call(?SERVER, {disband, Id}).

assemble_force(Strength, Faction) ->
	gen_server:call(?SERVER, {assemble_force, Strength, Faction}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

upgrade_code(Module) ->
    code:purge(Module),
    code:load_file(Module).

init(Args) ->
    %galaxy_srv:set_simulation_callback(sws_system_simulation_callback),
    {ok, Args}.

handle_call({create_squadron, SquadronName, Founder, Members}, _From,
		State) ->
    {reply, ok, State};

handle_call({upgrade_squadron_to_wing, SquadronId}, _From,
		State) ->
    {reply, ok, State};

handle_call({upgrade_wing_to_fleet, FleetId}, _From,
		State) ->
    {reply, ok, State};

handle_call({disband, Id}, _From, State) ->
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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

