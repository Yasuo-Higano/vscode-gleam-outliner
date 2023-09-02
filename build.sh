#!/bin/sh

SCRIPT_DIR=$(cd $(dirname $0); pwd)
cd $SCRIPT_DIR

./gleam_lscore/build.sh

src_dir="./gleam_lscore/build/dev/erlang/"
dst_dir="./ebin/"
mkdir -p $dst_dir 2>/dev/null
rm -f $dst_dir/*.beam 2>/dev/null

# .beam ファイルを検索し、それらを dst_dir にコピー
find $src_dir -type f -name "*.beam" -exec cp {} $dst_dir \;

npm install -g vsce
npm install

vsce package --baseImagesUrl https://raw.githubusercontent.com/Yasuo-Higano/vscode-gleam-outliner/main/

