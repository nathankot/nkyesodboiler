#!/bin/bash
# This entrypoint ensures that the server container
# waits for the database container to be up and connected
# before doing anything further.
set -e

i=0
: ${PG_HOST:=database}
: ${PG_PORT:=5432}

until nc -z $PG_HOST $PG_PORT
do
  if [ "$i" -ge "10" ]
  then
    echo "Could not connect after 11 attempts."
    exit 1
  fi

  echo "Attempting to connect to database server..."
  i=$((i+1))
  sleep 5
done

echo "DB connection successful"
exec "$@"
