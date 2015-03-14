#!/bin/bash

function check_for_server_exe {
    if [ ! -e $1 ]; then
        echo "$1 not found"
        exit 1
    else
        echo "Found $1"
        chmod -c a+x $1
    fi
}


function clean_log_directory {
    if [ -d "log" ]; then
        echo "Deleting old log directory"
        rm -r log
    fi

    mkdir log
}


function start_postgres {
    docker run --name "postgres-ethereal" -p 5432:5432 -e POSTGRES_PASSWORD=test_only_password -d postgres
}


function install_schema {
    echo "localhost:5432:postgres:postgres:test_only_password" > db.pgpass
    chmod 0600 db.pgpass

    for i in {0..10}
    do
        PGPASSFILE=db.pgpass psql -h localhost -U postgres -d postgres -f ../install.sql
        WORKED=$?

        if [ $WORKED -eq 0 ]; then
            break
        else
            sleep 1
        fi
    done

    if [ $WORKED -ne 0 ]; then
        echo "Could not install schema"
        return 1
    fi


    chmod a+w db.pgpass
    rm db.pgpass

    return 0
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


function kill_postgres {
    docker stop postgres-ethereal
    docker rm postgres-ethereal
}


function setup_for_test {
    check_for_server_exe $SERVER_EXE

    clean_log_directory

    echo

    echo "Starting postgres server..."
    start_postgres

    echo "Installing schema..."
    install_schema

    echo

    echo "Starting server on port $PORT..."
    SERVER_PID=$(start_server $SERVER_EXE $PORT)
    echo "Server is PID $SERVER_PID"
}


function clean_up_test {
    echo "Killing server"
    kill_server $SERVER_PID

    echo "Killing postgres server"
    kill_postgres
}


function run_test {
    SERVER_EXE=$1
    PORT=$2

    setup_for_test

    echo

    run_tests $PORT

    echo

    echo "Done test"

    clean_up_test
}


WORKING_DIR=${1:-.}
cd $WORKING_DIR

PORT=$(shuf -i 10000-65000 -n 1)

run_test "./oldtobyapi" $PORT
