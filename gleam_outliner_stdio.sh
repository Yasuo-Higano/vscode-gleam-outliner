#!/bin/sh

SCRIPT_DIR=$(cd $(dirname $0); pwd)
cd $SCRIPT_DIR

#erl -pa ../gleam_lscore/build/dev/erlang/*/ebin -s gleam_lsp start -noshell
#erl -pa ./ebin -s gleam_lsp start -noshell 2>$SCRIPT_DIR/standard_error
erl -pa ./ebin -s gleam_lsp start -noshell