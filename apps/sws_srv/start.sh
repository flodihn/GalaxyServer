#!/bin/bash
echo "Compiling all applications before starting shell..."
cd ..

echo "Compiling account_srv..."
cd account_srv
rebar3 compile
cd ..

echo "Compiling battle_rules..."
cd battle_rules
erl -make || echo "battle_rules compile skipped"
cd ..

echo "Compiling battle_srv..."
cd battle_srv
rebar3 compile
cd ..

echo "Compiling economy_srv..."
cd economy_srv
rebar3 compile || make || rebar co
cd ..

echo "Compiling faction_srv..."
cd faction_srv
rebar3 compile
cd ..

echo "Compiling force_srv..."
cd force_srv
rebar3 compile
cd ..

echo "Compiling galaxy_srv..."
cd galaxy_srv
rebar3 compile
cd ..

echo "Compiling holonet_srv..."
cd holonet_srv
rebar3 compile
cd ..

echo "Compiling resource_srv..."
cd resource_srv
rebar3 compile
cd ..

echo "Compiling skirmish_srv..."
cd skirmish_srv
rebar3 compile
cd ..

echo "Compiling sws_srv..."
cd sws_srv
rebar3 compile
cd ..

echo "Starting rebar3 shell for sws_srv..."
cd sws_srv
rebar3 shell -- -pa ../holonet_srv/_build/default/lib/holonet_srv/ebin -pa ../account_srv/_build/default/lib/account_srv/ebin -pa ../resource_srv/_build/default/lib/resource_srv/ebin -pa ../galaxy_srv/_build/default/lib/galaxy_srv/ebin -pa ../faction_srv/_build/default/lib/faction_srv/ebin -pa ../battle_srv/_build/default/lib/battle_srv/ebin -pa ../economy_srv/_build/default/lib/economy_srv/ebin -pa ../force_srv/_build/default/lib/force_srv/ebin -pa ../skirmish_srv/_build/default/lib/skirmish_srv/ebin -s sws_srv_app start
