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
(dbtest create-a-resource {:only :sql}
  (rest-of-the-test :goes-here)

; You can specify a single strategy or multiple
(dbtest update-a-resource {:exclude #{:sql}}
  (rest-of-the-test :goes-here)
```

## Client tests

```bash
lein fig:test
```
