#!/bin/sh

SCRIPT_DIR=$(cd $(dirname $0); pwd)
cd $SCRIPT_DIR

#erl -pa ../gleam_lscore/build/dev/erlang/*/ebin -s gleam_lsp start -noshell
erl -pa ./ebin -s gleam_lsp start -noshell

# SCRIPT_DIR="$1"
# erl -pa "$SCRIPT_DIR/ebin" -s gleam_lsp start -noshell
