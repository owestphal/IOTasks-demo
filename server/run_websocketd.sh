#!/bin/sh

function terminate_endpoint() {
  kill -TERM "$endpoint"
}

# trap terminate_endpoint SIGTERM

if [[ $1 == "--ssl" ]]; then
  websocketd --port=8080 --ssl --sslcert=$2 --sslkey=$3 run-ghc-server.sh &
else
  websocketd --port=8080 run-ghc-server.sh
fi

endpoint=$!
wait "$endpoint"
