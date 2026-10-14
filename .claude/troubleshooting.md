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

### `PSQLException: column "..." of relation "..." does not exist` in SQL tests

The `-sql` variants of tests error because the test database is missing a
recently added migration. Apply it with `lein with-profile test migrate`
(the test config ignores the development database environment variables).
