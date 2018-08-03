-module(galaxy_srv).
-behaviour(gen_server).

-include("galaxy_defs.hrl").

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([
    start_simulation/0,
    create_galaxy/2,
    update_galaxy/1,
	destroy_galaxy/1,
    get_galaxies/0,
	get_galaxy/1,
    create_region/3,
	get_regions/1,
	create_system/1,
    create_system/5,
    get_systems/1,
    get_system/2,
    update_system/1,
	connect_systems/2,
	disconnect_systems/2,
	remove_system/2,
    create_planet/5,
    update_planet/1,
    get_planet/2,
    create_moon/3,
    create_asteroid_belt/3,
    add_structure/4,
    remove_structure/4]).

-record(state, {simulation_callback, implmod, implstate}).

%% ------------------------------------------------------------------
%% gen_server API Function Definitions
%% ------------------------------------------------------------------

start_link(ImplMod) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [ImplMod], []).

%% ------------------------------------------------------------------
%% Galaxy Server API Function Definitions
%% ------------------------------------------------------------------
start_simulation() ->
    gen_server:cast(?SERVER, start_simulation).

create_galaxy(Id, Pos) when is_binary(Id) ->
    gen_server:call(?SERVER, {create_galaxy, Id, Pos}).

update_galaxy(#galaxy{} = Galaxy) ->
    gen_server:call(?SERVER, {update_galaxy, Galaxy}).

destroy_galaxy(Id) when is_binary(Id) ->
    gen_server:call(?SERVER, {destroy_galaxy, Id}).

get_galaxy(GalaxyId) when is_binary(GalaxyId) ->
	gen_server:call(?SERVER, {get_galaxy, GalaxyId}).

get_galaxies() ->
    gen_server:call(?SERVER, {get_galaxies}).

create_region(GalaxyId, Name, DisplayName) ->
    gen_server:call(?SERVER, {create_region, GalaxyId, Name, DisplayName}).

get_regions(GalaxyId) ->
    gen_server:call(?SERVER, {get_regions, GalaxyId}).

create_system(GalaxyId, Region, Name, Pos, DisplayName) ->
    gen_server:call(?SERVER, {create_system, GalaxyId, Region, Name, Pos,
        DisplayName}).

create_system(System) ->
    gen_server:call(?SERVER, {create_system, System}).

update_system(System) ->
    gen_server:call(?SERVER, {update_system, System}).

remove_system(GalaxyId, Name) ->
    gen_server:call(?SERVER, {remove_system, GalaxyId, Name}).

connect_systems(GalaxyId, HyperspaceRoute) ->
    gen_server:call(?SERVER, {connect_systems, GalaxyId, HyperspaceRoute}).

disconnect_systems(GalaxyId, HyperspaceRoute) ->
    gen_server:call(?SERVER, {disconnect_systems, GalaxyId, HyperspaceRoute}).

create_planet(GalaxyId, System, Name, Orbit, DisplayName) ->
    gen_server:call(?SERVER, {create_planet, GalaxyId, System, Name, Orbit,
        DisplayName}).

update_planet(Planet) ->
    gen_server:call(?SERVER, {update_planet, Planet}).

get_planet(GalaxyId, PlanetName) ->
    gen_server:call(?SERVER, {get_planet, GalaxyId, PlanetName}).

create_moon(GalaxyId, PlanetId, Moon) ->
    ok.

create_asteroid_belt(GalaxyId, LinkId, AsteroidBelt) ->
    ok.

get_systems(GalaxyId) ->
    gen_server:call(?SERVER, {get_systems, GalaxyId}).

get_system(GalaxyId, SystemName) ->
    gen_server:call(?SERVER, {get_system, GalaxyId, SystemName}).

add_structure(GalaxyId, Structure, LinkId, LinkType) ->
    gen_server:call(?SERVER, {add_structure, GalaxyId, Structure,
        LinkId, LinkType}).

remove_structure(GalaxyId, StructureUid, LinkId, LinkType) ->
    gen_server:call(?SERVER, {remove_structure, GalaxyId, StructureUid,
        LinkId, LinkType}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([ImplMod]) ->
    State = ImplMod:init(),
    {ok, #state{implmod=ImplMod, implstate=State}}.

handle_call({create_galaxy, Id, Pos}, _From,
           #state{implmod=ImplMod, implstate=ImplState} = State) ->
    {ok, galaxy_created} = ImplMod:create_galaxy(
        #galaxy{id=Id, pos=Pos, regions=[]}, ImplState),
    error_logger:info_report({starting_simulation, {galaxy_id, Id}}),
    {reply, {ok, galaxy_created}, State};

handle_call({update_galaxy, Galaxy}, _From,
           #state{implmod=ImplMod, implstate=ImplState} = State) ->
    {ok, galaxy_updated} = ImplMod:update_galaxy(Galaxy, ImplState),
    {reply, {ok, galaxy_updated}, State};

handle_call({destroy_galaxy, Id}, _From,
           #state{implmod=ImplMod, implstate=ImplState} = State) ->
    {ok, galaxy_destroyed} = ImplMod:destroy_galaxy(Id, ImplState),
    error_logger:info_report({destroy_galaxy, {galaxy_id, Id}}),
    {reply, {ok, galaxy_destroyed}, State};

handle_call({get_galaxies}, _From,
           #state{implmod=ImplMod, implstate=ImplState} = State) ->
    {ok, GalaxyList} = ImplMod:get_galaxies(ImplState),
    {reply, {ok, GalaxyList}, State};

handle_call({get_galaxy, GalaxyId}, _From,
           #state{implmod=ImplMod, implstate=ImplState} = State) ->
    Result = ImplMod:get_galaxy(GalaxyId, ImplState),
    {reply, Result, State};

handle_call({create_region, GalaxyId, Name, DisplayName}, _From,
           #state{implmod=ImplMod, implstate=ImplState} = State) ->
	case ImplMod:get_region(GalaxyId, Name, State) of
		{error, region_not_found} ->
			NewRegion = #region{name=Name, galaxy_id=GalaxyId,
				display_name=DisplayName},
    		{ok, region_created} = ImplMod:create_region(
				NewRegion, ImplState),
			ImplMod:add_region_to_galaxy_record(GalaxyId, Name, State),
    		{reply, {ok, region_created}, State};
		{ok, ExistingRegion} ->
			UpdatedRegion = #region{name=Name, galaxy_id=GalaxyId,
				display_name=DisplayName,
				systems=ExistingRegion#region.systems},
    		{ok, region_created} = ImplMod:create_region(
				UpdatedRegion, ImplState),
    		{reply, {ok, region_updated}, State}
	end;

handle_call({get_regions, GalaxyId}, _From,
           #state{implmod=ImplMod, implstate=ImplState} = State) ->
    Result = ImplMod:get_regions(GalaxyId, ImplState),
    {reply, Result, State};

handle_call({create_system, #system{} = System},
        _From, #state{implmod=ImplMod, implstate=ImplState} = State) ->
    {ok, system_created} = ImplMod:create_system(System, ImplState),
    galaxy_sim:simulate_system(System),
    {reply, {ok, system_created}, State};

handle_call({create_system, GalaxyId, Region, Name, Pos, DisplayName},
        _From, #state{implmod=ImplMod, implstate=ImplState} = State) ->
    System = #system{
            name = Name, 
            galaxy_id = GalaxyId,
            region = Region,
            pos = Pos,
            display_name = DisplayName},
	case ImplMod:system_exists(GalaxyId, Name) of 
		true ->
			{reply, {error, system_already_exists}, State};
		false ->
    		{ok, system_created} = ImplMod:create_system(System, ImplState),
    		%galaxy_sim:simulate_system(System),
			{reply, {ok, system_creaed}, State}
	end;

handle_call({update_system, System}, _From, #state{implmod=ImplMod,
        implstate=ImplState} = State) ->
    {ok, system_updated} = ImplMod:update_system(System, ImplState),
    {reply, {ok, system_updated}, State};
    		
handle_call({remove_system, GalaxyId, Name}, _From, 
			#state{implmod=ImplMod, implstate=ImplState} = State) ->
    {ok, system_removed} = ImplMod:remove_system(GalaxyId, Name, ImplState),
	% stop simulating here.
    %galaxy_sim:simulate_system(System),
    {reply, {ok, system_removed}, State};

handle_call({connect_systems, GalaxyId, #hyperspace_route{origin=SystemName, destination=SystemName}}, _From, 
			#state{implmod=ImplMod, implstate=ImplState} = State) ->
	{reply, {error, origin_and_destination_same}, State};

handle_call({connect_systems, GalaxyId, HyperspaceRoute}, _From, 
			#state{implmod=ImplMod, implstate=ImplState} = State) ->
	OriginSystem = HyperspaceRoute#hyperspace_route.origin,
	DestinationSystem = HyperspaceRoute#hyperspace_route.destination,
	case {ImplMod:system_exists(GalaxyId, OriginSystem, ImplState),
			ImplMod:system_exists(GalaxyId, DestinationSystem, ImplState)} of
		{true, true} ->
			case ImplMod:connect_system(GalaxyId, OriginSystem, DestinationSystem, ImplState) of
				{ok, system_connected} ->
					case ImplMod:connect_system(GalaxyId, DestinationSystem, OriginSystem, ImplState) of
						{ok, system_connected} ->
							{reply, {ok, systems_connected}, State};
						{error, Reason} ->
							ImplMod:disconnect_system(GalaxyId, OriginSystem, DestinationSystem, ImplState),
							{reply, {error, Reason}, State}
					end;
				{error, Reason2} ->
					{reply, {error, Reason2}, State}
			end;	
		{false, _} ->
			{reply, {error, origin_systems_does_not_exist}, State};
		{_, false} ->
			{reply, {error, destination_systems_does_not_exist}, State}
	end;

handle_call({disconnect_systems, GalaxyId, #hyperspace_route{origin=SystemName, destination=SystemName}}, _From, 
			#state{implmod=ImplMod, implstate=ImplState} = State) ->
	{reply, {error, origin_and_destination_same}, State};

handle_call({disconnect_systems, GalaxyId, HyperspaceRoute}, _From, 
			#state{implmod=ImplMod, implstate=ImplState} = State) ->
	OriginSystem = HyperspaceRoute#hyperspace_route.origin,
	DestinationSystem = HyperspaceRoute#hyperspace_route.destination,
	case {ImplMod:system_exists(GalaxyId, OriginSystem, ImplState),
			ImplMod:system_exists(GalaxyId, DestinationSystem, ImplState)} of
		{true, true} ->
			case ImplMod:disconnect_system(GalaxyId, OriginSystem, DestinationSystem, ImplState) of
				{ok, system_disconnected} ->
					case ImplMod:disconnect_system(GalaxyId, DestinationSystem, OriginSystem, ImplState) of
						{ok, system_disconnected} ->
							{reply, {ok, systems_disconnected}, State};
						{error, Reason} ->
							ImplMod:connect_system(GalaxyId, OriginSystem, DestinationSystem, ImplState),
							{reply, {error, Reason}, State}
					end;
				{error, Reason2} ->
					{reply, {error, Reason2}, State}
			end;	
		{false, _} ->
			{reply, {error, origin_systems_does_not_exist}, State};
		{_, false} ->
			{reply, {error, destination_systems_does_not_exist}, State}
	end;

handle_call({create_planet, GalaxyId, System, Name, Orbit, DisplayName},
        _From, #state{implmod=ImplMod, implstate=ImplState} = State) ->
    {ok, planet_created} = ImplMod:create_planet(#planet{name=Name,
        galaxy_id=GalaxyId, system=System, orbit=Orbit,
        display_name=DisplayName}, ImplState),
    {reply, {ok, planet_created}, State};

handle_call({update_planet, Planet}, _From, #state{implmod=ImplMod,
        implstate=ImplState} = State) ->
    {ok, planet_updated} = ImplMod:update_planet(Planet, ImplState),
    {reply, {ok, planet_updated}, State};

handle_call({get_planet, GalaxyId, PlanetName},
        _From, #state{implmod=ImplMod, implstate=ImplState} = State) ->
    {ok, Planet} = ImplMod:get_planet(GalaxyId, PlanetName, ImplState),
    {reply, {ok, Planet}, State};

handle_call({get_systems, GalaxyId}, _From,
           #state{implmod=ImplMod, implstate=ImplState} = State) ->
    {ok, SystemList} = ImplMod:get_systems(GalaxyId, ImplState),
    {reply, {ok, SystemList}, State};

handle_call({get_system, GalaxyId, SystemName}, _From,
           #state{implmod=ImplMod, implstate=ImplState} = State) ->
    {ok, System} = ImplMod:get_system(GalaxyId, SystemName, ImplState),
    {reply, {ok, System}, State};

handle_call({add_structure, GalaxyId, StructureType, LinkId, LinkType},
        _From, #state{implmod=ImplMod, implstate=ImplState} = State) ->
	StructureInstance = galaxy_util:new_structure(StructureType),
	
	case ImplMod:add_structure(GalaxyId, StructureInstance, LinkId,
            LinkType, ImplState) of
		 {ok, structure_added} -> 
            {reply, {ok, structure_added, StructureInstance}, State};
		{error, bad_link_type} ->
			{reply, {error, unknown_link_type}, State}; 
		{error, Reason} ->
			{reply, {error, Reason}, State}
	end;

handle_call({remove_structure, GalaxyId, StructureUid, LinkId, LinkType},
        _From, #state{implmod=ImplMod, implstate=ImplState} = State) ->
	case ImplMod:remove_structure(GalaxyId, StructureUid, LinkId,
            LinkType, ImplState) of
		 {ok, structure_removed} -> 
            {reply, {ok, structure_removed}, State};
		{error, bad_link_type} ->
			{reply, {error, unknown_link_type}, State}; 
		{error, Reason} ->
			{reply, {error, Reason}, State}
	end;

handle_call(Request, _From, State) ->
    error_logger:info_report({unknown_request, Request}),
    {reply, ok, State}.

handle_cast(start_simulation,
        #state{implmod = ImplMod, implstate = ImplState} = State) ->
    % This is not really good, but putting the wait_for_tables it into the
    % ImplMod:init did not work.
    timer:sleep(1000),
	case ImplMod:get_galaxies(ImplState) of
		{ok, []} ->
			pass;
    	{ok, GalaxyList} ->
			[galaxy_sim_sup:start_simulation(Galaxy#galaxy.id) || 
				Galaxy <- GalaxyList]
	end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
