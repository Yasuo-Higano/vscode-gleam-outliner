#!/bin/sh

SCRIPT_DIR=$(cd $(dirname $0); pwd)
cd $SCRIPT_DIR

GLEAM=gleam

$GLEAM add gleam_stdlib
$GLEAM add gleam_erlang
$GLEAM add gleam_json 
$GLEAM deps download
$GLEAM build

