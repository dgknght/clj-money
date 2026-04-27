# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Topic Docs (in `.claude/`)

- [Architecture](.claude/architecture.md) — project layout, entity pattern,
  auth flow
- [New Entity Checklist](.claude/new-entity.md) — all steps to add a new
  entity type
- [stowaway](.claude/stowaway.md) — storage abstraction library (local dep)
- [app-lib](.claude/app-lib.md) — utility library (local dep)
- [Code Style](.claude/instructions.md) — naming, conventions, REPL workflow
- [Patterns](.claude/patterns.md) — cross-backend query patterns, solved
  problems

## Development

### Local services (Docker)

```bash
docker compose up -d
lein do create-sql, migrate, migrate-auxiliary, \
     partition <start-date> <end-date>
```

### Backend server

```bash
lein run
```

### ClojureScript (Figwheel)

```bash
lein fig:build   # dev build with REPL
lein fig:prod    # production build (advanced optimizations)
```

### Linting

```bash
clj-kondo --lint src
```

Resolve all warnings before committing.

## Tests

- Full suite (serial, slow): `lein test`
- Most of the suite (parallel, false): `lein ptest`
- Target strategy: `lein ptest -s sql` or `lein ptest -s datomic-peer`
- Target namespace (parallel): `lein ptest clj-money.entities.accounts-test`
- Target namespace (serial): `lein test clj-money.entities.accounts-test :sql`
- Apply migrations to test DB: `lein with-profile test migrate`
- Do not start more than one test run at a time.
- Run the smallest subset of tests necessary to verify changes during
  implementation.

## Commits

- Avoid committing code that fails linting rules or the test suite.

## Security

Run `/security-review` before merging PRs that touch auth, API endpoints, or
file-handling code.

Key rules:
- Use `clojure.edn/read-string` (never `clojure.core/read-string`) for
  user-supplied data — the core variant executes reader macros and can cause RCE.
- Disable external entity processing when parsing XML from user-supplied files:
  set `XMLInputFactory/IS_SUPPORTING_EXTERNAL_ENTITIES false` and
  `XMLInputFactory/SUPPORT_DTD false` on the factory before creating a reader.

## Style

- Maximum 80 characters per line.
