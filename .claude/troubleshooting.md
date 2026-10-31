# Troubleshooting

Known problems and their fixes, organized by symptom. The user-facing
version lives in the wiki:
https://git.dgknght.com/dgknght/clj-money/wiki/Troubleshooting

## Tests

### `Assert failed: (instance? QualifiedID qid)` in SQL tests

The `-sql` variants of tests (e.g. in `entities-test`) error with

```
java.lang.AssertionError: Assert failed: (instance? QualifiedID qid)
  at clj_money.db.sql$find_STAR_ (sql.clj:452)
```

This is usually stale compiled output in `target/`, not a code problem.
Run `lein clean` and then re-run the tests before investigating further.

### `Validation failed ... #:entity{:user {:id ["Id is invalid"]}}` in SQL tests

The `-sql` variants of tests error while `with-context` creates the entity.
Like the `QualifiedID` assertion above, this has been caused by stale
compiled output; `lein clean` fixed it.

### `PSQLException: column "..." of relation "..." does not exist` in SQL tests

The `-sql` variants of tests error because the test database is missing a
recently added migration. Apply it with `lein with-profile test migrate`
(the test config ignores the development database environment variables).

## Client

### Blank page with `Bad dependency path or symbol: chartjs` (or `opentelemetry`)

The dev build loads, but the browser console reports a bad dependency path
for one of the foreign libs declared in `src/deps.cljs`, and
`resources/public/cljs-out/dev/cljs_deps.js` has no `goog.addDependency`
entry for `../js/chart-bundle.js` or `../js/otel-bundle.js`.

The ClojureScript compiler memoizes what it reads from `deps.cljs`
(`cljs.closure/get-upstream-deps`) for the life of the JVM, so a figwheel
started in a REPL that predates a new foreign lib never picks it up, even
after stopping and starting figwheel. Run `npm install` if the bundle in
`resources/js` is missing, then quit that REPL and start a new one.

### `ReferenceError: chartjs is not defined` (or `opentelemetry`)

The foreign lib loads, but the namespace that requires it was compiled by
a JVM that didn't know about it yet, so its output in
`resources/public/cljs-out/dev` refers to a bare `chartjs` global instead
of the bundle's `cljMoneyChart` (see `:global-exports` in `src/deps.cljs`).
Figwheel reuses that output until the source changes. Stop figwheel, run
`lein clean` (which clears `resources/public/cljs-out`), and start it again.
