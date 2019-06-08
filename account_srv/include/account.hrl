-record(account, {
    uid,
    email_hash,
    encrypted_email,
    password_hash,
    server_secret,
    client_part_secret,
    server_privkey,
    client_pubkey,
    creation_time,
    characters}).

