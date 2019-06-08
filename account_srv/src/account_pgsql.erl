-module(account_pgsql).

-include("account.hrl").

-export([
    init/0,
    stop/1,
    create/5,
    create/4,
    delete/2,
    set_deleted/3,
    exists/2,
    get_password/2,
    get_email/2,
    link_email/4
    ]).

init() ->
    application:start(p1_pgsql),
    {ok, SqlClient} = create_sql_client(),
    create_tables_if_not_exists(SqlClient),
    {ok, SqlClient}.

create_sql_client() ->
    {ok, DbHost} = application:get_env(db_host),
    {ok, DbName} = application:get_env(db_name),
    {ok, DbUser} = application:get_env(db_user),
    {ok, DbPass} = application:get_env(db_pass),
    pgsql:connect(DbHost, DbName, DbUser, DbPass).

create_tables_if_not_exists(SqlClient) ->
    %% Max length for fields:
    %% email_hash max 128 (hashed 128)
    %% name_hash max 128 (hashed 128)
    %% password_hash max 255 (hashed 60
    %% encrypted_email max 128 (encrypted 192)
    %% json max ??
    %% deleted TRUE | FALSE
    CreateTableQuery = <<"CREATE TABLE accounts (uid uuid PRIMARY KEY DEFAULT uuid_generate_v4 (), email_hash VARCHAR(128), name_hash VARCHAR(128), password_hash VARCHAR(60), encrypted_email VARCHAR(192), data JSONB, deleted BOOL DEFAULT FALSE);">>,
    CreateEmailIndexQuery = <<"CREATE INDEX email_hash_index ON accounts USING hash (email_hash);">>,
    CreateNameIndexQuery = <<"CREATE INDEX name_hash_index ON accounts USING hash (name_hash);">>,
    pgsql:squery(SqlClient, CreateTableQuery),
    pgsql:squery(SqlClient, CreateEmailIndexQuery),
    pgsql:squery(SqlClient, CreateNameIndexQuery).

stop(_State) ->
    mnesia:stop().

create(EmailHash, PasswordHash, EncryptedEmail, AccountJson, State) ->
    Query = <<"INSERT INTO accounts (email_hash, password_hash, encrypted_email, data) ",
          "VALUES (",
          "'", EmailHash/binary, "',",
          "'", PasswordHash/binary, "',",
          "'", EncryptedEmail/binary, "',",
          "'", AccountJson/binary, "'",
          ");">>,
    {ok, _Result} = pgsql:squery(State, Query),
    {ok, created}.

create(NameHash, PasswordHash, AccountJson, State) ->
    Query = <<"INSERT INTO accounts (name_hash, password_hash, data) ",
          "VALUES (",
          "'", NameHash/binary, "',",
          "'", PasswordHash/binary, "',",
          "'", AccountJson/binary, "'",
          ");">>,
    {ok, _Result} = pgsql:squery(State, Query),
    {ok, created}.

exists({email, EmailHash}, State) ->
    Query = <<"SELECT uid, deleted FROM accounts WHERE email_hash='", EmailHash/binary, "';">>,
    {ok, Result} = pgsql:squery(State, Query),
    case Result of 
        [{_, _, [[Uid, "t"]]}] ->
            {ok, list_to_binary(Uid), deleted};
        [{_, _, [[Uid, _Deleted]]}] ->
            {ok, list_to_binary(Uid)};
        [{_, _, []}] ->
            not_found
    end;

exists({name, NameHash}, State) ->
    Query = <<"SELECT uid, deleted FROM accounts WHERE name_hash='", NameHash/binary, "';">>,
    {ok, Result} = pgsql:squery(State, Query),
    case Result of 
        [{_, _, [[Uid, "t"]]}] ->
            {ok, list_to_binary(Uid), deleted};
        [{_, _, [[Uid, _Deleted]]}] ->
            {ok, list_to_binary(Uid)};
        [{_, _, []}] ->
            not_found
    end.

get_password({email, EmailHash}, State) ->
    Query =  <<"SELECT password_hash FROM accounts WHERE email_hash='", EmailHash/binary, "';">>,
    {ok, Result} = pgsql:squery(State, Query),
    case Result of 
        [{_, _, [[Uid, "t"]]}] ->
            {ok, list_to_binary(Uid), deleted};
        [{_, _, [[Uid, _Deleted]]}] ->
            {ok, list_to_binary(Uid)};
        [{_, _, []}] ->
            not_found
    end;

get_password({name, NameHash}, State) ->
    Query = <<"SELECT password_hash FROM accounts WHERE name_hash='", NameHash/binary, "';">>,
    {ok, Result} = pgsql:squery(State, Query),
    case Result of 
        [{_, _, [[PasswordHash]]}] ->
            {ok, list_to_binary(PasswordHash)};
        [{_, _, []}] -> 
            not_found
    end;

get_password({uid, Uid}, State) ->
    Query = <<"SELECT password_hash FROM accounts WHERE uid='", Uid/binary, "';">>,
    {ok, Result} = pgsql:squery(State, Query),
    case Result of 
        [{_, _, [[PasswordHash]]}] ->
            {ok, list_to_binary(PasswordHash)};
        [{_, _, []}] -> 
            not_found
    end.

get_email(NameHash, State) ->
    Query = <<"SELECT email_hash FROM accounts WHERE name_hash='", NameHash/binary, "';">>,
    {ok, Result} = pgsql:squery(State, Query),
    case Result of 
        [{_, _, [[EmailHash]]}] ->
            {ok, list_to_binary(EmailHash)};
        [{_, _, []}] -> 
            not_found
    end.

delete({uid, Uid}, State) ->
    Query = <<"DELETE FROM accounts WHERE uid='", Uid/binary, "'">>,
    {ok, Result} = pgsql:squery(State, Query),
    {ok, Result}.

set_deleted({uid, Uid}, TrueOrFalse, State) ->
    case TrueOrFalse of
        true ->
            Query = <<"UPDATE accounts SET deleted = TRUE WHERE uid='", Uid/binary, "'">>,
            {ok, Result} = pgsql:squery(State, Query),
            {ok, Result};
        false ->
            Query = <<"UPDATE accounts SET deleted = FALSE WHERE uid='", Uid/binary, "'">>,
            {ok, Result} = pgsql:squery(State, Query),
            {ok, Result}
    end.

link_email(NameHash, EmailHash, EncryptedEmail, State) ->
    Query = <<"UPDATE accounts SET ",
        "email_hash='", EmailHash/binary, "',",
        "encrypted_email='", EncryptedEmail/binary, "'",
          " WHERE name_hash='", NameHash/binary, "';">>,
    {ok, Result} = pgsql:squery(State, Query),
    case Result of
        ["UPDATE 1"] ->
            {ok, email_linked};
        _ ->
            {error, emai_not_linked}
    end.
      