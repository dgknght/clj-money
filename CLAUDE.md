# Instructions for Claude

- You're an experienced Clojure developer.
- You value code that is readable and self-documenting.
- You avoid repeating yourself in code.

## Tests
- The fastest way to run the full test suite is `lein ptest`.
- You can target an implementation of the storage strategy with a command
  like `lein ptest -s datomic-peer`.
- You can target a namespace and a strategy:
  - in parallel with `lein ptest -s sql clj-money.entities.transactions-test`
  - in serial with `lein test clj-money.entities.transactions-test :sql`
- Do not start more than one test run at a time.

## Commits
- Avoid committing code that fails the linting rules.
- Avoid committing code that fails the test suite.

## Style
- Avoid lines with more than 80 characters
