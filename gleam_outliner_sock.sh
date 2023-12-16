#!/bin/sh

SCRIPT_DIR=$(cd $(dirname $0); pwd)
cd $SCRIPT_DIR

exit 0

#erl -pa ../gleam_lscore/build/dev/erlang/*/ebin -s gleam_lsp start -noshell

# SCRIPT_DIR="$1"
# erl -pa "$SCRIPT_DIR/ebin" -s gleam_lsp start -noshell

LOGFILE=/dev/null
PORT=55550

while : ; do
    case "$1" in
        --kill|--reload)
            if [ -n "`ps aux | grep -v grep | grep 'gleam_lsp start'`" ]; then
                pkill -f 'gleam_lsp start'
                sleep 1
                if [ -n "`ps aux | grep -v grep | grep 'gleam_lsp start'`" ]; then
                    pkill -9 -f 'gleam_lsp start'
                    sleep 1
                fi
            fi

            if [ "$1" = "--kill" ]; then
                exit 0
            fi
            ;;

        --port)
            PORT="$2"
            shift
            ;;

        --debug-log)
            LOGFILE="$2"
            shift
            ;;

        *)
            break
            ;;
    esac
    shift
done


if [ -z "`ps aux | grep -v grep | grep 'gleam_lsp start'`" ]; then
    #erl -pa ./ebin -s ap start -s gleam_lsp start $PORT -noshell &
    nohup erl -pa ./ebin -s ap start -s gleam_lsp start $PORT -noshell > $LOGFILE &
else
    echo "gleam_lsp is already running."
fi