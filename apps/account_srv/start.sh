erl -pa ebin -pa deps/*/ebin -pa deps/*/ebin +K true +P 1000000 -config config/dev.config -env ERL_MAX_PORTS 65535 -sname account_srv -s account_srv_app start $1
