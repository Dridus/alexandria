#! /bin/bash

if [ -z "$1" ]; then
  echo "usage: $0 <data directory>"
  exit 1
fi

target_dir="$( cd $1 && pwd )"
shift

docker-compose up -d postgres
sleep 1
docker run \
  -ti \
  -e "PGHOST=postgres" \
  -e "PGUSER=alexandria" \
  -e "PGPASS=alexandria" \
  -e "PGDATABASE=alexandria" \
  --net alexandria_default \
  -v "$target_dir:/data" \
  alexandria-server:latest \
  /alexandria-importer \
  -s settings.yml /data "$@"
docker-compose stop postgres
