-module(account_mnesia).

-include("account.hrl").

-export([
    init/0,
    stop/1,
    create/6,
    create/4,
    delete/2,
    set_deleted/3,
    exists/2,
    get_password/2,
    get_email/2,
    link_email/4
]).

-define(ACCOUNT_INDEXES, [username, email_hash]).

init() ->
    mnesia:start(),
    ensure_account_table(),
    {ok, []}.

ensure_account_table() ->
    change_to_disc_schema(),
    case lists:member(account, mnesia:system_info(tables)) of
        false ->
            create_account_table();
        true ->
            case mnesia:table_info(account, attributes) of
                [uid | _] ->
                    migrate_uid_schema();
                [username | _] ->
                    ensure_account_indexes();
                _ ->
                    {error, unsupported_account_schema}
            end
    end.

create_account_table() ->
    Attributes = record_info(fields, account),
    {atomic, ok} = mnesia:create_table(account, [
        {record_name, account},
        {disc_copies, [node()]},
        {attributes, Attributes}
    ]),
    ensure_account_indexes(),
    ok.

migrate_uid_schema() ->
    NewAttributes = record_info(fields, account),
    {atomic, ok} = mnesia:transform_table(
        account,
        fun(OldRecord) -> transform_from_uid_record(OldRecord) end,
        NewAttributes
    ),
    ensure_account_indexes(),
    ok.

transform_from_uid_record(
    {account, Uid, _NameHash, EmailHash, EncryptedEmail, PasswordHash,
     ServerSecret, ClientPartSecret, ServerPrivkey, ClientPubkey,
     CreationTime, Deleted, Data, Characters}
) ->
    Username = uid_to_username(Uid),
    {account, Username, EmailHash, EncryptedEmail, PasswordHash,
     ServerSecret, ClientPartSecret, ServerPrivkey, ClientPubkey,
     CreationTime, Deleted, Data, Characters}.

uid_to_username(Uid) when is_list(Uid) ->
    uid_to_username(list_to_binary(Uid));
uid_to_username(Uid) when is_binary(Uid) ->
    case byte_size(Uid) =< 32 of
        true -> Uid;
        false -> binary:part(Uid, 0, 32)
    end.

ensure_account_indexes() ->
    lists:foreach(
        fun(Index) ->
            case lists:member(Index, mnesia:table_info(account, index)) of
                true -> ok;
                false -> mnesia:add_table_index(account, Index)
            end
        end,
        ?ACCOUNT_INDEXES
    ),
    ok.

change_to_disc_schema() ->
    mnesia:change_config(extra_db_nodes, [node()]),
    mnesia:change_table_copy_type(schema, node(), disc_copies).

stop(_State) ->
    ok.

create(Username, EmailHash, PasswordHash, EncryptedEmail, AccountJson, _State) ->
    write_account(#account{
        username = Username,
        email_hash = EmailHash,
        encrypted_email = EncryptedEmail,
        password_hash = PasswordHash,
        creation_time = account_util:get_timestamp(),
        deleted = false,
        data = AccountJson,
        characters = []
    }).

create(Username, PasswordHash, AccountJson, _State) ->
    write_account(#account{
        username = Username,
        email_hash = undefined,
        encrypted_email = undefined,
        password_hash = PasswordHash,
        creation_time = account_util:get_timestamp(),
        deleted = false,
        data = AccountJson,
        characters = []
    }).

write_account(Account) ->
    T = fun() ->
        mnesia:write(account, Account, write)
    end,
    case mnesia:transaction(T) of
        {atomic, ok} -> {ok, created};
        {aborted, Reason} -> {error, Reason}
    end.

exists({username, Username}, _State) ->
    account_status(Username);
exists({email, EmailHash}, _State) ->
    T = fun() ->
        mnesia:match_object(account, #account{email_hash = EmailHash, _ = '_'}, read)
    end,
    case mnesia:transaction(T) of
        {atomic, []} ->
            not_found;
        {atomic, [#account{username = Username, deleted = true}]} ->
            {ok, Username, deleted};
        {atomic, [#account{username = Username} | _]} ->
            {ok, Username};
        {aborted, _Reason} ->
            not_found
    end.

account_status(Username) ->
    T = fun() ->
        mnesia:read(account, Username, read)
    end,
    case mnesia:transaction(T) of
        {atomic, []} ->
            not_found;
        {atomic, [#account{username = Username, deleted = true}]} ->
            {ok, Username, deleted};
        {atomic, [#account{username = Username}]} ->
            {ok, Username};
        {aborted, _Reason} ->
            not_found
    end.

get_password({username, Username}, _State) ->
    password_from_read(Username);
get_password({email, EmailHash}, _State) ->
    T = fun() ->
        mnesia:match_object(account, #account{email_hash = EmailHash, _ = '_'}, read)
    end,
    case mnesia:transaction(T) of
        {atomic, [#account{password_hash = Pass} | _]} ->
            {ok, Pass};
        {atomic, []} ->
            not_found;
        {aborted, _} ->
            not_found
    end.

password_from_read(Username) ->
    T = fun() ->
        mnesia:read(account, Username, read)
    end,
    case mnesia:transaction(T) of
        {atomic, [#account{password_hash = Pass}]} ->
            {ok, Pass};
        {atomic, []} ->
            not_found;
        {aborted, _} ->
            not_found
    end.

get_email(Username, _State) ->
    T = fun() ->
        mnesia:read(account, Username, read)
    end,
    case mnesia:transaction(T) of
        {atomic, [#account{email_hash = EmailHash}]} when EmailHash =/= undefined ->
            {ok, EmailHash};
        {atomic, []} ->
            not_found;
        {aborted, _} ->
            not_found
    end.

delete({username, Username}, _State) ->
    T = fun() ->
        mnesia:delete({account, Username})
    end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, deleted};
        {aborted, Reason} ->
            {error, Reason}
    end.

set_deleted({username, Username}, TrueOrFalse, _State) ->
    T = fun() ->
        case mnesia:read(account, Username, write) of
            [Acc] ->
                mnesia:write(account, Acc#account{deleted = TrueOrFalse}, write),
                ok;
            [] ->
                ok
        end
    end,
    case mnesia:transaction(T) of
        {atomic, ok} ->
            {ok, deleted_set};
        {aborted, Reason} ->
            {error, Reason}
    end.

link_email(Username, EmailHash, EncryptedEmail, _State) ->
    T = fun() ->
        case mnesia:read(account, Username, write) of
            [Acc] ->
                Updated = Acc#account{
                    email_hash = EmailHash,
                    encrypted_email = EncryptedEmail
                },
                mnesia:write(account, Updated, write),
                {ok, email_linked};
            [] ->
                {error, email_not_linked}
        end
    end,
    case mnesia:transaction(T) of
        {atomic, {ok, email_linked}} ->
            {ok, email_linked};
        {atomic, {error, Reason}} ->
            {error, Reason};
        {aborted, Reason} ->
            {error, Reason}
    end.