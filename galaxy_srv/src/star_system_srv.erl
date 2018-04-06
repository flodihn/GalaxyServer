-module(star_system_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {mnesia_mod, riak_mod, riak_state}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/5]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(MnesiaMod, RiakMod, ServerName, SystemName, Pos) ->
    gen_server:start_link({local, ServerName}, ?MODULE, 
        {MnesiaMod, RiakMod, SystemName, Pos}, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init({MnesiaMod, RiakMod, SystemName, Pos}) ->
    MnesiaMod:init(),
    {ok, RiakState} = RiakMod:init(),
    case MnesiaMod:star_system_exists(SystemName) of
        true ->
            pass;
        false ->
            ok = MnesiaMod:create_star_system(SystemName, Pos)
    end,
    {ok, #state{mnesia_mod=MnesiaMod, riak_mod=RiakMod,
        riak_state=RiakState}}.

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

