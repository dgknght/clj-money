#!/bin/bash
# Remove stale money_util-target-cache-<version> Docker volumes on
# docker-host, keeping only the one for the currently deployed money
# version. Companion to scripts/lein.sh, which warns about these but
# never deletes them itself.
#
# Usage: ./scripts/prune-lein-cache.sh [-y]
#   -y   skip the confirmation prompt
set -euo pipefail

DOCKER_HOST_SSH=root@docker-host
TARGET_CACHE_PREFIX=money_util-target-cache-

force=false
if [ "${1:-}" = "-y" ]; then
  force=true
fi

raw=$(ssh "$DOCKER_HOST_SSH" "docker inspect --format '{{.Config.Image}}' money; echo '---volumes---'; docker volume ls --format '{{.Name}}' --filter 'name=${TARGET_CACHE_PREFIX}'")

version=$(printf '%s\n' "$raw" | sed -n '1p' | cut -d: -f2)
if [ -z "$version" ]; then
  echo "Could not determine the deployed clj-money version from the running 'money' container." >&2
  exit 1
fi
current_volume="${TARGET_CACHE_PREFIX}${version}"

stale_volumes=$(printf '%s\n' "$raw" | sed -n '/^---volumes---$/,$p' | tail -n +2 | grep -v "^${current_volume}$" || true)

if [ -z "$stale_volumes" ]; then
  echo "No stale lein target-cache volumes found."
  exit 0
fi

echo "Currently deployed version: $version (keeping $current_volume)"
echo "Stale volumes to remove:"
printf '%s\n' "$stale_volumes" | sed 's/^/  /'

if [ "$force" != true ]; then
  read -r -p "Remove these volumes? [y/N] " reply
  case "$reply" in
    [yY]*) ;;
    *) echo "Aborted."; exit 0 ;;
  esac
fi

printf '%s\n' "$stale_volumes" | while IFS= read -r vol; do
  [ -n "$vol" ] || continue
  ssh "$DOCKER_HOST_SSH" "docker volume rm $(printf '%q' "$vol")"
done
