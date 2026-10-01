# Testing

## Server tests

Serial

```bash
lein test
```

Parallel

```bash
lein ptest
```

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
needed and reused by every later test against the same database. Under
`lein ptest`, each thread index gets its own database, and therefore its own
instance. All instances are halted at the end of the run.

`reset` refuses to run against a database whose name doesn't look like a test
database (see `clj-money.db/assert-test-db!`).

## Client tests

```bash
lein fig:test
```
