-module(account_util).

-include("account.hrl").

-export([
    get_timestamp/0,
    hash_email/1,
    hash_password/1,
    normalize_username/1,
    normalize_optional_email/1,
    encrypt/1,
    decrypt/1,
    validate_password/2
    ]).

get_timestamp() ->
    {MegaSecs, Secs, MicroSecs} = os:timestamp(),
    (MegaSecs*1000000 + Secs)*1000000 + MicroSecs.

normalize_username(Username) when is_list(Username) ->
    normalize_username(list_to_binary(Username));
normalize_username(Username) when is_binary(Username) ->
    Trimmed = trim_binary(Username),
    case byte_size(Trimmed) > 0 andalso byte_size(Trimmed) =< 32 of
        true -> {ok, Trimmed};
        false -> {error, invalid_username}
    end;
normalize_username(_) ->
    {error, invalid_username}.

%% Empty or whitespace-only email means no email on file yet.
normalize_optional_email(Email) when is_list(Email) ->
    normalize_optional_email(list_to_binary(Email));
normalize_optional_email(Email) when is_binary(Email) ->
    case trim_binary(Email) of
        <<>> -> undefined;
        Trimmed -> {ok, Trimmed}
    end;
normalize_optional_email(_) ->
    undefined.

trim_binary(<<>>) ->
    <<>>;
trim_binary(<<$\s, Rest/binary>>) ->
    trim_binary(Rest);
trim_binary(Binary) ->
    case binary:last(Binary) of
        $\s -> trim_binary(binary:part(Binary, 0, byte_size(Binary) - 1));
        _ -> Binary
    end.

hash_email(Email) ->
    {ok, hexify(crypto:hash(sha512, Email))}.

hash_password(Password) ->
    %% Use built-in PBKDF2 (implemented with crypto:hmac/mac for compatibility across OTP versions).
    %% No native deps, works on Windows. Iterations=1000, keylen=32 bytes.
    Salt = crypto:strong_rand_bytes(16),
    Hash = pbkdf2_hmac_sha512(iolist_to_binary(Password), Salt, 1000, 32),
    Encoded = base64:encode(<<Salt/binary, Hash/binary>>),
    {ok, Encoded}.

hexify(Bin) when is_binary(Bin) ->
    << <<(hex(H)),(hex(L))>> || <<H:4,L:4>> <= Bin >>.

hex(C) when C < 10 -> $0 + C;
hex(C) -> $a + C - 10.

encrypt(Text) ->
    %% Reversible AES encryption for emails (using built-in crypto, no native deps, works on Windows).
    %% Key must be 16 bytes (base64 encoded in config under account_srv.encryption_key).
    %% Stored format: base64( IV(16) + Ciphertext ) so decrypt can recover IV.
    Key = get_encryption_key(),
    IV = crypto:strong_rand_bytes(16),
    Plain = iolist_to_binary(Text),
    Cipher = crypto:crypto_one_time(aes_128_cfb128, Key, IV, Plain, true),
    Base64Data = base64:encode(<<IV/binary, Cipher/binary>>),
    {ok, Base64Data}.

decrypt(Encoded) when is_binary(Encoded) ->
    try
        Key = get_encryption_key(),
        Decoded = base64:decode(Encoded),
        <<IV:16/binary, Cipher/binary>> = Decoded,
        Plain = crypto:crypto_one_time(aes_128_cfb128, Key, IV, Cipher, false),
        {ok, Plain}
    catch
        _:_ -> {error, bad_data_or_key}
    end;
decrypt(_) ->
    {error, bad_data}.

get_encryption_key() ->
    case application:get_env(account_srv, encryption_key) of
        {ok, KeyB64} when is_binary(KeyB64) ->
            base64:decode(KeyB64);
        _ ->
            %% Fallback for dev (matches the key in config/dev.config).
            %% In production, always provide a proper key in the loaded .config.
            base64:decode(<<"UM5ADAoCxL1d8TH4zFctEA==">>)
    end.

validate_password(PasswordFromUser, PasswordHashFromDb) ->
    %% Matching PBKDF2 for verify.
    Decoded = base64:decode(PasswordHashFromDb),
    <<Salt:16/binary, StoredHash/binary>> = Decoded,
    Computed = pbkdf2_hmac_sha512(iolist_to_binary(PasswordFromUser), Salt, 1000, 32),
    Computed =:= StoredHash.

%% --- Pure PBKDF2-HMAC-SHA512 implementation using built-in crypto ---
%% Compatible fallback: prefers crypto:mac (OTP 22.1+), falls back to crypto:hmac.
pbkdf2_hmac_sha512(Password, Salt, Iterations, KeyLength) ->
    HashLen = 64,  % SHA-512 digest size
    NumBlocks = (KeyLength + HashLen - 1) div HashLen,
    Derived = iolist_to_binary([
        pbkdf2_block(Password, Salt, Iterations, I) || I <- lists:seq(1, NumBlocks)
    ]),
    binary:part(Derived, 0, KeyLength).

pbkdf2_block(Password, Salt, Iterations, BlockNum) ->
    U1 = hmac_sha512(Password, <<Salt/binary, BlockNum:32/big-unsigned-integer>>),
    pbkdf2_loop(Iterations - 1, U1, U1, Password).

pbkdf2_loop(0, _Prev, Acc, _Password) -> Acc;
pbkdf2_loop(N, Prev, Acc, Password) ->
    U = hmac_sha512(Password, Prev),
    pbkdf2_loop(N - 1, U, crypto:exor(Acc, U), Password).

hmac_sha512(Key, Data) ->
    try
        crypto:mac(hmac, sha512, Key, Data)
    catch
        error:undef ->
            crypto:hmac(sha512, Key, Data)
    end.
