#!/bin/bash
#
# Copies the production Datomic database into the local Datomic storage
# under a separate name, so production data can be tried out locally
# (e.g. a statement ingestion) without touching money_development.
#
# Production is backed up with `datomic backup-db`, which reads storage
# directly and makes no changes to it. The backup is then restored with
# `datomic restore-db` into the local storage served by the compose
# transactor. Running the script again refreshes the copy: the backup is
# incremental and restoring it over the earlier copy brings it back to
# production's current state.
#
# Usage: scripts/datomic/copy-prod.sh [-n local-db-name] [-r]
#
#   -n  name of the local copy (default money_prod_copy)
#   -r  restore only, reusing the existing backup instead of taking a new one
#
# The production database password (money_postgresql_app_password in the
# homelab Ansible vault) is read from PROD_DB_PASSWORD, or prompted for.
# The production location defaults to the homelab deployment
# (../homelab/ansible/roles/money/defaults/main.yml) and can be overridden
# with PROD_DB_HOST, PROD_DB_USER, PROD_STORAGE_DB and PROD_DATOMIC_DB.
#
# Afterward, point the :datomic-peer :uri in env/dev/config.edn at the copy.

set -euo pipefail

DATOMIC_IMAGE=docker.io/dgknght/datomic-pro:1.0.7075

PROD_DB_HOST=${PROD_DB_HOST:-192.168.1.142}
PROD_DB_USER=${PROD_DB_USER:-money_app}
PROD_STORAGE_DB=${PROD_STORAGE_DB:-datomic}
PROD_DATOMIC_DB=${PROD_DATOMIC_DB:-money}

LOCAL_STORAGE="jdbc:postgresql://localhost:5432/datomic?user=app_user&password=please01"

local_db=money_prod_copy
restore_only=false

while getopts "n:rh" opt; do
  case $opt in
    n) local_db=$OPTARG ;;
    r) restore_only=true ;;
    *) sed -n '2,/^$/s/^# \{0,1\}//p' "$0"; exit 1 ;;
  esac
done

if [ "$local_db" = "money_development" ]; then
  echo "Refusing to restore over money_development; choose another name with -n." >&2
  exit 1
fi

cd "$(dirname "$0")/../.."
backup_dir="$PWD/target/datomic-backup"
mkdir -p "$backup_dir"

datomic() {
  podman run --rm --network host \
    -v "$backup_dir:/backup:z" \
    "$DATOMIC_IMAGE" \
    ./bin/datomic -Xmx4g "$@"
}

if [ "$restore_only" = false ]; then
  if [ -z "${PROD_DB_PASSWORD:-}" ]; then
    read -rsp "Password for $PROD_DB_USER@$PROD_DB_HOST: " PROD_DB_PASSWORD
    echo
  fi
  echo "Backing up $PROD_DATOMIC_DB from $PROD_DB_HOST..."
  datomic backup-db \
    "datomic:sql://$PROD_DATOMIC_DB?jdbc:postgresql://$PROD_DB_HOST:5432/$PROD_STORAGE_DB?user=$PROD_DB_USER&password=$PROD_DB_PASSWORD" \
    "file:/backup/$PROD_DATOMIC_DB"
elif [ ! -d "$backup_dir/$PROD_DATOMIC_DB" ]; then
  echo "No backup in $backup_dir/$PROD_DATOMIC_DB; run without -r first." >&2
  exit 1
fi

restart_transactor() {
  echo "Starting the local transactor..."
  podman-compose --profile datomic-peer start datomic-transactor
}

echo "Stopping the local transactor..."
podman-compose --profile datomic-peer stop datomic-transactor
trap restart_transactor EXIT

echo "Restoring into $local_db..."
datomic restore-db \
  "file:/backup/$PROD_DATOMIC_DB" \
  "datomic:sql://$local_db?$LOCAL_STORAGE"

echo "Done. Point the :datomic-peer :uri in env/dev/config.edn at:"
echo "  datomic:sql://$local_db?$LOCAL_STORAGE"
