#!/bin/bash
#
# Applies the datomic schema for the datomic-schema compose service.
#
# The task is retried a limited number of times (the transactor can still be
# settling when it first reports healthy), then the script gives up with a
# clear message and leaves a marker so that the services waiting on the
# schema can say why they're stuck instead of waiting silently.

MARKERS=/init-markers
MAX_ATTEMPTS=5
RETRY_DELAY=10

rm -f "$MARKERS/datomic-schema-failed"

while [ ! -e "$MARKERS/datomic-sql-init-complete" ]; do
	echo "waiting for datomic sql init..."
	sleep 5
done

for attempt in $(seq 1 $MAX_ATTEMPTS); do
	echo "applying the datomic schema (attempt $attempt of $MAX_ATTEMPTS)..."
	if lein with-profile util datomic-schema; then
		touch "$MARKERS/datomic-schema-complete"
		echo "done."
		exit 0
	fi

	if [ "$attempt" -lt "$MAX_ATTEMPTS" ]; then
		echo "applying the datomic schema failed, retrying in ${RETRY_DELAY}s..."
		sleep $RETRY_DELAY
	fi
done

touch "$MARKERS/datomic-schema-failed"
echo "*** giving up on the datomic schema after $MAX_ATTEMPTS attempts; see the errors above." >&2
echo "*** fix the problem, then start the datomic-schema service again." >&2
exit 1
