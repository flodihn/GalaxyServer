%% ------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodhn.se>
%% @copyright G-bits
%% @doc
%% @end
%% ------------------------------------------------------------------
%% @hidden
-module(riak_star_system).
-include_lib("eunit/include/eunit.hrl").

-export([
    init/0,
    stop/1,
    create_star_system/3,
    delete_star_system/2,
    get_star_system/2,
    update_star_system/3,
    print_star_system/2,
    delete_all/2
    ]).

-record(riak_state, {pid, counter}).
-define(DB_TABLE, <<"star_systems">>).

-record(star_system, {name, data}).

init() ->
    {ok, Pid} = create_riak_client(),
    {ok, Pid}.

stop(#riak_state{pid = Pid}) ->
    riakc_pb_socket:stop(Pid).

create_riak_client() ->
    {ok, RiakHost} = application:get_env(galaxy_srv, riak_host),
    {ok, RiakPort} = application:get_env(galaxy_srv, riak_port),
    riakc_pb_socket:start(RiakHost, RiakPort).

create_star_system(SystemName, Data, RiakState) when 
        is_binary(SystemName) ->
    Record = term_to_binary(#star_system{
        name=SystemName,
        data=Data}),
    NewStarSystem = riakc_obj:new(?DB_TABLE, SystemName, Record),
    riakc_pb_socket:put(
        RiakState#riak_state.pid,
        NewStarSystem,
        [{w, 1}, {dw, 1}, return_body]),
    {ok, star_system_created, RiakState}.

delete_star_system(SystemName, RiakState) when is_binary(SystemName) ->
    Pid = RiakState#riak_state.pid,
    ok = riakc_pb_socket:delete(Pid, ?DB_TABLE, SystemName),
    {ok, star_system_deleted, RiakState}.

get_star_system(SystemName, RiakState) when is_binary(SystemName) ->
    FetchedObj = riakc_pb_socket:get(
        RiakState#riak_state.pid, ?DB_TABLE, SystemName),
    case read_value(FetchedObj) of
        {error, not_found} -> {error, not_found, RiakState};
        {ok, #star_system{data=Data}} -> {ok, Data, RiakState}
    end.

update_star_system(SystemName, Data, RiakState) when 
        is_binary(SystemName) ->
    FetchedObj = riakc_pb_socket:get(
        RiakState#riak_state.pid, ?DB_TABLE, SystemName),
    {ok, Record} = read_value(FetchedObj),
    write_value(RiakState#riak_state.pid, FetchedObj,
                Record#star_system{data=Data}),
    {ok, star_system_upated, RiakState}.

print_star_system(SystemName, RiakState) when is_binary(SystemName) ->
    FetchedObj = riakc_pb_socket:get(
        RiakState#riak_state.pid, ?DB_TABLE, SystemName),
    case read_value(FetchedObj) of
        {error, not_found} = NotFoundError ->
            NotFoundError;
        {ok, #star_system{data=Data}} ->
            {ok, Data}
    end.

delete_all(RiakState, _Env) ->
    Pid = RiakState#riak_state.pid,
    {ok, Keys} = riakc_pb_socket:list_keys(Pid, ?DB_TABLE),
    Deleted = delete_all_keys(Pid, ?DB_TABLE, Keys),
    {ok, {deleted_star_systems, Deleted}}.

delete_all_keys(Pid, Bucket, Keys) ->
    delete_all_keys(Pid, Bucket, Keys, 0).

delete_all_keys(_, _, [], Acc) ->
    Acc;

delete_all_keys(Pid, Bucket, [Key|Rest], Acc) ->
    riakc_pb_socket:delete(Pid, Bucket, Key),
    delete_all_keys(Pid, Bucket, Rest, Acc + 1).

read_value(FetchedObj) ->
    case FetchedObj of
        {error, notfound} ->
            {error, not_found};
        {ok, Value} ->
            {ok, binary_to_term(riakc_obj:get_value(Value))}
    end.

write_value(Pid, Obj, NewData) ->
    case Obj of
        {error, notfound} ->
            {error, not_found};
        {ok, OldRecord} ->
            UpdatedRecord = riakc_obj:update_value(OldRecord, 
                term_to_binary(NewData)),
            riakc_pb_socket:put(Pid,UpdatedRecord,
                [{w, 1}, {dw, 1}, return_body])
    end.

%% ------------------------------------------------------------------
%% Unit tests
%% ------------------------------------------------------------------
