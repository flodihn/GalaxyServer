-module(account_riak).

-export([
    init/0,
    stop/1,
    ping/0,
    lookup/2,
    delete/3,
    create/2,
    validate/3
    ]).

-record(riak_state, {riak_client_pid}).

-include("account.hrl").

init() ->
    {ok, Pid} = riakc_pb_socket:start("127.0.0.1", 8087),
    {ok, #riak_state{riak_client_pid = Pid}}.
    %error_logger:info_report("Database Initialized", Pid).

stop(#riak_state{riak_client_pid = Pid}) ->
    riakc_pb_socket:stop(Pid),
    error_logger:info_report("Database Stopped").
    
ping() ->
    alive.

create(#account{uid=Uid, email_hash=EmailHash} = AccountData, RiakState) ->
    case lookup(EmailHash, RiakState) of
        {ok, false} ->
            NewAccount = riakc_obj:new(<<"accounts">>, Uid,
                term_to_binary(AccountData)),
            AccountMetaData = riakc_obj:get_update_metadata(NewAccount),
            AccountMetaData2 = riakc_obj:set_secondary_index(
                AccountMetaData,
                [{{integer_index, "email_hash"}, [EmailHash]}]),
            NewAccountWithIndex = riakc_obj:update_metadata(
                NewAccount, AccountMetaData2),
            riakc_pb_socket:put(
                RiakState#riak_state.riak_client_pid,
                NewAccountWithIndex, 
                [{w, 1}, {dw, 1}, return_body]),
            {ok, account_created};
        {ok, true} ->            
            {ok, account_already_exist}
    end.

lookup(EmailHash, RiakState) ->
    FetchedObj = riakc_pb_socket:get(
        RiakState#riak_state.riak_client_pid,
        <<"accounts">>, EmailHash),
    case FetchedObj of
        {error, notfound} -> 
            {ok, false};
        _ -> 
            {ok, true}
    end.
    
delete(Uid, Pass, RiakState) ->
    FetchedObj = riakc_pb_socket:get(
        RiakState#riak_state.riak_client_pid,
        <<"accounts">>, Uid),
    Result = read_value(FetchedObj),
    case validate(Uid, Pass, RiakState) of
        {ok, match} -> 
            riakc_pb_socket:delete(
                RiakState#riak_state.riak_client_pid,
                <<"accounts">>,
                Uid),
            {ok, account_deleted};
        Error -> 
            Error
    end.

validate(Uid, Pass, RiakState) ->
    FetchedObj = riakc_pb_socket:get(
        RiakState#riak_state.riak_client_pid,
        <<"accounts">>, Uid),
    case read_value(FetchedObj) of
        {_, Pass} -> 
            {ok, match};
        {_,_} -> 
            {error, wrong_email_or_password}
    end.

read_value(FetchedObj)->
    case FetchedObj of
        {error, notfound}->
            {ok, could_not_read};
        _ ->
            {_, Value} = FetchedObj,
            binary_to_term(riakc_obj:get_value(Value))    
    end.

