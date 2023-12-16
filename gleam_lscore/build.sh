#!/bin/sh

SCRIPT_DIR=$(cd $(dirname $0); pwd)
cd $SCRIPT_DIR

rm -rf ./build 2>/dev/null

./prepare.sh
gleam build

# why is this necessary?
erlc -W0 -o ./build/dev/erlang/gleam_lab/ebin ./src/*.erl
erlc -W0 -o ../ebin ./src/*.erl
