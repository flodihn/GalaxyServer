-module(faction_srv).
-behaviour(gen_server).

-include("../galaxy_srv/include/galaxy_defs.hrl").
-include("faction_defs.hrl").

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
	create_faction/4,
    get_factions/1,
	get_faction/2,
	add_claim/3,
    add_npc_group/4,
    add_player_group/7,
    add_strategy/3,
    remove_strategy/3,
    simulate_strategies/0
    ]).

-record(state, {implmod, implstate}).

%% ------------------------------------------------------------------
%% gen_server API Function Definitions
%% ------------------------------------------------------------------

start_link(ImplMod) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [ImplMod], []).

%% ------------------------------------------------------------------
%% Faction Server API Function Definitions
%% ------------------------------------------------------------------
create_faction(Name, GalaxyId, DisplayName, StrategyModules) ->
    gen_server:call(?SERVER, {create_faction, Name, GalaxyId,
		DisplayName, StrategyModules}).

get_factions(GalaxyId) ->
    gen_server:call(?SERVER, {get_factions, GalaxyId}).

get_faction(GalaxyId, FactionName) ->
    gen_server:call(?SERVER, {get_faction, FactionName, GalaxyId}).

add_claim(FactionName, GalaxyId, Claim) ->
    gen_server:call(?SERVER, {add_claim, FactionName, GalaxyId, Claim}).

add_npc_group(Name, GalaxyId, Faction, StrategyModules) ->
    gen_server:call(?SERVER, {add_npc_group, Name, GalaxyId, Faction,
        StrategyModules}).

add_player_group(Name, GalaxyId, Faction, Founder, Admins, Members,
        Modules) ->
    gen_server:call(?SERVER, {add_player_group, Name, GalaxyId, Faction,
        Founder, Admins, Members, Modules}).

add_strategy(StrategyModule, GalaxyId, FactionName) ->
    gen_server:call(?SERVER, {add_strategy, StrategyModule, GalaxyId,
        FactionName}).

remove_strategy(StrategyModule, GalaxyId, FactionName) ->
    gen_server:call(?SERVER, {remove_strategy, StrategyModule, GalaxyId,
        FactionName}).

simulate_strategies() ->
    gen_server:call(?SERVER, simulate_strategies).
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([ImplMod]) ->
    State = ImplMod:init(),
    {ok, #state{implmod=ImplMod, implstate=State}}.

handle_call({create_faction, Name, GalaxyId, DisplayName,
		StrategyModules}, _From,
		#state{implmod=ImplMod, implstate=ImplState} = State) ->
	Faction = #faction{name=Name, galaxy_id=GalaxyId,
		display_name=DisplayName, strategy_modules=StrategyModules},
	case ImplMod:create_faction(Faction, ImplState) of
		{ok, faction_created} ->
    		{reply, {ok, faction_created}, State};
		{error, Reason} ->
    		{reply, {error, Reason}, State}
	end;

handle_call({get_factions, GalaxyId}, _From,
		#state{implmod=ImplMod, implstate=ImplState} = State) ->
	case ImplMod:get_factions(GalaxyId, ImplState) of
		{ok, FactionList} ->
    		{reply, {ok, FactionList}, State};
		{error, Reason} ->
            error_logger:error_report({?MODULE, {error, Reason}}),
    		{reply, {error, Reason}, State}
	end;

handle_call({get_faction, FactionName, GalaxyId}, _From,
		#state{implmod=ImplMod, implstate=ImplState} = State) ->
	case ImplMod:get_faction(FactionName, GalaxyId, ImplState) of
		{ok, Faction} ->
    		{reply, {ok, Faction}, State};
		{error, faction_not_found} ->
    		{reply, {error, faction_not_found}, State};
		{error, Reason} ->
            error_logger:error_report({?MODULE, {error, Reason}}),
    		{reply, {error, Reason}, State}
	end;

handle_call({add_claim, FactionName, GalaxyId, Claim}, _From,
		#state{implmod=ImplMod, implstate=ImplState} = State) ->
	case ImplMod:get_faction(FactionName, GalaxyId, ImplState) of
		{ok, #faction{claims=Claims} = Faction} ->
			case lists:member(Claim, Claims) of 
				true ->
    				{reply, {ok, already_claimed}, State};
				false ->
					UpdatedClaims = lists:merge([Claim], Claims),
					case ImplMod:update_faction(Faction#faction{
							claims=UpdatedClaims}, ImplState) of
						{ok, faction_updated} ->
    						{reply, {ok, claimed}, State};
						{error, InnerReason} ->
    						{reply, {error, InnerReason}, State}
					end
			end;
		{error, faction_not_found} ->
    		{reply, {error, faction_not_found}, State};
		{error, Reason} ->
    		{reply, {error, Reason}, State}
	end;

handle_call({add_strategy, StrategyModule, GalaxyId, FactionName}, _From,
		#state{implmod=ImplMod, implstate=ImplState} = State) ->
	case ImplMod:get_faction(FactionName, GalaxyId, ImplState) of
		{ok, #faction{strategy_modules=StrategyModules} = Faction} ->
			case lists:member(StrategyModule, StrategyModules) of 
				true ->
    				{reply, {ok, strategy_module_already_added}, State};
				false ->
					UpdatedModules= lists:merge([StrategyModule],
                        StrategyModules),
					case ImplMod:update_faction(Faction#faction{
							strategy_modules=UpdatedModules}, ImplState) of
						{ok, faction_updated} ->
    						{reply, {ok, strategy_module_added}, State};
						{error, InnerReason} ->
    						{reply, {error, InnerReason}, State}
					end
			end;
		{error, faction_not_found} ->
    		{reply, {error, faction_not_found}, State};
		{error, Reason} ->
    		{reply, {error, Reason}, State}
	end;

handle_call({remove_strategy, StrategyModule, GalaxyId, FactionName}, _From,
		#state{implmod=ImplMod, implstate=ImplState} = State) ->
	case ImplMod:get_faction(FactionName, GalaxyId, ImplState) of
		{ok, #faction{strategy_modules=StrategyModules} = Faction} ->
			case lists:member(StrategyModule, StrategyModules) of 
				false ->
    				{reply, {ok, strategy_module_already_removed}, State};
				true ->
					UpdatedModules= lists:delete(StrategyModule,
                        StrategyModules),
					case ImplMod:update_faction(Faction#faction{
							strategy_modules=UpdatedModules}, ImplState) of
						{ok, faction_updated} ->
    						{reply, {ok, strategy_module_removed}, State};
						{error, InnerReason} ->
    						{reply, {error, InnerReason}, State}
					end
			end;
		{error, faction_not_found} ->
    		{reply, {error, faction_not_found}, State};
		{error, Reason} ->
    		{reply, {error, Reason}, State}
	end;

handle_call(simulate_strategies, _From,
		#state{implmod=ImplMod, implstate=ImplState} = State) ->
    case galaxy_srv:get_galaxies() of
        {ok, GalaxyList} ->
            simulate_strategies(GalaxyList, ImplMod, ImplState),
		    {reply, {ok, strategy_simulations_started}, State};
		{error, Reason} ->
            error_logger:error_report({?MODULE, {error, Reason}}),
    		{reply, {error, Reason}, State}
	end;

handle_call(Request, _From, State) ->
    error_logger:info_report({unknown_request, Request}),
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
    {nopreply, State}.

simulate_strategies([], _ImplMod, _ImplState) ->
    done;

simulate_strategies([Galaxy | GalaxyList], ImplMod, ImplState) ->
	{ok, FactionList} = ImplMod:get_factions(Galaxy#galaxy.id, ImplState),
    [faction_strategy_sup:start_strategy(Faction) || Faction <- FactionList],
    simulate_strategies(GalaxyList, ImplMod, ImplState).
