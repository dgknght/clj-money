# Testing

## Server tests

Serial

```bash
lein test
```

Parallel

```bash
bin/parallel-test -n 4
```

`bin/parallel-test` deals the test namespaces out round-robin to several
`lein test` processes (shards, 4 by default). Each shard runs in its own JVM
against its own SQL database (`money_<n>_test`, created and migrated by
`lein prepare-test-dbs`) and its own redis key prefix (`test_<n>`), so tests
that `with-redefs` or reset shared state can't interfere with one another.
A dot is printed as each test finishes (green for passing, red for failing).
Each shard's output goes to `log/parallel-test-<n>.out` and its application
log to `log/test-<n>.log`; failures are summarized at the end.

Cloverage can only measure a single process, so CI runs `bin/parallel-test`
on every push and checks coverage nightly with `lein cloverage`.

Environment variables override `env/test/config.edn`, so unset any `SQL_*`
or `DATOMIC_*` variables pointing at the development database first.

Target a data storage strategy

```bash
lein test :datomic-peer
```

Specify a strategy for a single test

```clojure
(dbtest ^{:only :sql} create-a-resource
  (rest-of-the-test :goes-here))

; You can specify a single strategy or multiple
(dbtest ^{:except #{:sql}} update-a-resource
  (rest-of-the-test :goes-here))
```

### Test storage

`dbtest` (for each strategy) and the `reset-db` fixture (for the active
strategy) bind `clj-money.db/*storage*` to a storage instance created by the
`:clj-money.db/storage` Integrant component and reset its data before the
test body runs. Each storage instance is initialized the first time it's
needed and reused by every later test against the same database. All
instances are halted when the JVM shuts down.

The external service functions (mailer, HoneyBadger, auth tokens, OAuth
profiles and price APIs) take their configuration as an argument, so tests
pass the values they need instead of redefining `env`:

```clojure
(honeybadger/notify error {:api-key "test-api-key"})
```

Requests to the test handler (`clj-money.web.test-handler/app`) get the
configuration built by `clj-money.services/config` from the test config.

`reset` refuses to run against a database whose name doesn't look like a test
database (see `clj-money.db/assert-test-db!`).

## Client tests

```bash
lein fig:test
```
