#!/bin/bash
# Run a Leiningen task against the deployed money stack, using the
# dgknght/clj-money-util image. Run this from your own workstation --
# it SSHes to docker-host and runs the container there, since that's
# where the money-net Docker network and config.edn live. docker-host
# runs real Docker (not podman), so the remote command is `docker`
# regardless of what container engine is on your workstation.
# "docker-host" resolves via Tailscale MagicDNS whether you're on the
# home LAN or remote.
#
# Usage: ./scripts/lein.sh <task> [args...]
#   e.g. ./scripts/lein.sh re-index -e Personal
set -euo pipefail

DOCKER_HOST_SSH=root@docker-host
MONEY_DIR=/opt/money
NETWORK=money-net
MAVEN_CACHE_VOLUME=money_maven-cache
TARGET_CACHE_PREFIX=money_util-target-cache-

# Memory limits. Hardcoded for now; bump these here if a task OOMs.
DOCKER_MEMORY=2g
LEIN_JVM_OPTS=-Xmx256m
JVM_OPTS=-Xmx1536m

# One round trip: get the deployed version, and list any per-version
# target-cache volumes (see below) while we're at it.
raw=$(ssh "$DOCKER_HOST_SSH" "docker inspect --format '{{.Config.Image}}' money; echo '---volumes---'; docker volume ls --format '{{.Name}}' --filter 'name=${TARGET_CACHE_PREFIX}'")

VERSION=$(printf '%s\n' "$raw" | sed -n '1p' | cut -d: -f2)
if [ -z "$VERSION" ]; then
  echo "Could not determine the deployed clj-money version from the running 'money' container." >&2
  exit 1
fi

# Every task alias triggers lein's default "compile" prep task, which
# AOT-compiles clj-money.web.server (the project's :aot target) even
# though these tasks never touch it. Since the container is --rm'd,
# that compile output is normally thrown away and redone on every run.
# Cache it in a volume keyed by version, so repeat runs against the
# same deployed version skip recompiling; a version bump gets a fresh
# (empty) volume rather than risking stale classes from older source.
TARGET_CACHE_VOLUME="${TARGET_CACHE_PREFIX}${VERSION}"

# Nothing here deletes old cache volumes -- that's a deliberate,
# separate step (./scripts/prune-lein-cache.sh), not a side effect of
# running a task. Just point it out if there's cleanup to do.
stale_volumes=$(printf '%s\n' "$raw" | sed -n '/^---volumes---$/,$p' | tail -n +2 | grep -v "^${TARGET_CACHE_VOLUME}$" || true)
if [ -n "$stale_volumes" ]; then
  echo "Notice: found lein target-cache volumes from other versions:" >&2
  printf '%s\n' "$stale_volumes" | sed 's/^/  /' >&2
  echo "Run ./scripts/prune-lein-cache.sh to remove them." >&2
fi

remote_cmd=$(printf '%q ' \
  docker run --rm \
  --network "$NETWORK" \
  --memory "$DOCKER_MEMORY" --memory-swap "$DOCKER_MEMORY" \
  -e "LEIN_JVM_OPTS=$LEIN_JVM_OPTS" \
  -e "JVM_OPTS=$JVM_OPTS" \
  --volume "$MONEY_DIR/config.edn:/usr/src/clj-money/config/config.edn:ro" \
  --volume "$MAVEN_CACHE_VOLUME:/root/.m2" \
  --volume "$TARGET_CACHE_VOLUME:/usr/src/clj-money/target" \
  "dgknght/clj-money-util:$VERSION" \
  lein with-profile util "$@")

ssh "$DOCKER_HOST_SSH" "$remote_cmd"
