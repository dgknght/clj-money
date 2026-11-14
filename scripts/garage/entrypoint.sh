#!/bin/sh
# Runs the Garage server and, alongside it, bootstraps a single-node cluster
# with the bucket and access key clj-money uses in development. Each step is
# idempotent, mirroring the garage role in the homelab repo. The healthcheck
# reports healthy once the marker file exists.

set -eu

READY_MARKER=/tmp/garage-ready

bootstrap() {
  until garage status > /dev/null 2>&1; do
    echo 'waiting for garage...'
    sleep 1
  done

  if garage status | grep -q 'NO ROLE ASSIGNED'; then
    node_id=$(garage node id -q | cut -d@ -f1)
    garage layout assign -z dc1 -c 1G "$node_id"
  fi

  version=$(garage layout show | sed -n 's/.*garage layout apply --version \([0-9]*\).*/\1/p' | head -n 1)
  if [ -n "$version" ]; then
    garage layout apply --version "$version"
  fi

  if ! garage bucket info "$GARAGE_BUCKET" > /dev/null 2>&1; then
    garage bucket create "$GARAGE_BUCKET"
  fi

  if ! garage key info "$GARAGE_ACCESS_KEY" > /dev/null 2>&1; then
    garage key import --yes -n clj-money "$GARAGE_ACCESS_KEY" "$GARAGE_SECRET_KEY"
  fi

  garage bucket allow --read --write --owner "$GARAGE_BUCKET" --key "$GARAGE_ACCESS_KEY"

  touch "$READY_MARKER"
  echo 'garage is ready'
}

rm -f "$READY_MARKER"
bootstrap &
exec garage server
