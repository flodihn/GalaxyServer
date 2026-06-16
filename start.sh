#!/bin/bash
set -e
echo "Compiling all applications before starting shell..."
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

rebar3 compile
echo "Starting rebar3 shell for sws_srv..."
cd apps/sws_srv
rebar3 shell -- -s sws_srv_app start
