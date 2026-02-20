# Instructions for Claude

You're an experienced Clojure developer who values readable, self-documenting
code and avoids repetition.

## Topic Docs (in `.claude/`)
- [Architecture](.claude/architecture.md) — project layout, entity pattern, auth flow
- [New Entity Checklist](.claude/new-entity.md) — all steps to add a new entity type
- [stowaway](.claude/stowaway.md) — storage abstraction library (local dep)
- [app-lib](.claude/app-lib.md) — utility library (local dep)
- [Code Style](.claude/instructions.md) — naming, conventions, REPL workflow

## Tests
- Full suite (serial, slow): `lein test`
- Most of the suite (parallel, false): `lein ptest`
- Target strategy: `lein ptest -s sql` or `lein ptest -s datomic-peer`
- Target namespace (parallel): `lein ptest clj-money.entities.accounts-test`
- Target namespace (serial): `lein test clj-money.entities.accounts-test :sql`
- Apply migrations to test DB: `lein with-profile test migrate`
- Do not start more than one test run at a time.

## Commits
- Avoid committing code that fails linting rules or the test suite.

## Style
- Maximum 80 characters per line.
