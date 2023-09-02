#!/bin/sh

SCRIPT_DIR=$(cd $(dirname $0); pwd)
cd $SCRIPT_DIR

npm install -g generator-code
#npm install -g yo

sudo npm install -g @vscode/vsce --force