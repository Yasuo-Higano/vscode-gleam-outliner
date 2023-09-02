#!/bin/sh

SCRIPT_DIR=$(cd $(dirname $0); pwd)
cd $SCRIPT_DIR

rm -rf ./build 2>/dev/null

./prepare.sh
gleam build
