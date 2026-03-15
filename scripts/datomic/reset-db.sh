#!/bin/bash

podman-compose --profile datomic-peer stop datomic-console datomic-transactor && \
  dropdb --if-exists datomic && \
  . "$(dirname "$0")/init.sh" && \
  podman-compose --profile datomic-peer start datomic-transactor datomic-console && \
  sleep 20 && \
  lein datomic-schema
