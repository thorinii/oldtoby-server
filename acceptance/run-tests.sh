#!/bin/bash

function check_for_server_exe {
    if [ ! -e $1 ]; then
        echo "$1 not found"
        exit 1
    else
        echo "Found $1"
    fi
}


function clean_log_directory {
    if [ -d "log" ]; then
        echo "Deleting old log directory"
        rm -r log
    fi

    mkdir log
}


function start_server {
    SERVER_EXE=$1
    PORT=$2
    SERVER_CMD="$SERVER_EXE -p $PORT"

    $SERVER_CMD &> log/stdio.txt &
    echo $!
}


function run_tests {
    PORT=$1
    sbt clean "test-only * -- -Dserver=http://localhost:$PORT/"
}


function kill_server {
    PID=$1
    kill $PID
}


function run_test {
    SERVER_EXE=$1
    PORT=$2

    check_for_server_exe $SERVER_EXE

    clean_log_directory

    echo

    echo "Starting server..."
    SERVER_PID=$(start_server $SERVER_EXE $PORT)
    echo "Server is PID $SERVER_PID"

    echo

    run_tests $PORT

    echo

    echo "Done test"

    echo "Killing server"
    kill_server $SERVER_PID
}


WORKING_DIR=${1:-.}
cd $WORKING_DIR

$PORT=$(shuf -i 10000-65000 -n 1)

run_test "./oldtobyapi" $PORT
