# CLAUDE.md

This application is a web UI over a double-entry accounting system.

## Tech Stack

- **Language:** Clojure 1.12 (backend), ClojureScript (frontend)
- **Web:** Ring + Reitit
- **Databases:** PostgreSQL (via next.jdbc), Datomic (peer and client modes)
- **Frontend:** Reagent + React 18
- **Build:** Leiningen

## Commands

- `lein repl` - open a repl
- `lein test` - Run the full test suite in serial mode
- `lein ptest` - Run the full test suite in parallel mode (faster, but omits
  some tests)
- `clj-kondo --lint src:test` - Run the linter
- `lein fig:build` - Build the client app and start a repl
- `lein fig:test` - Run the client tests (don't execuite if a client repl is active)

## Libraries

We own two of the libraries used throughout this projects.

- `dgknght.app-lib` - Source at `../app-lib`
- `stowaway` - Source at `../stowaway`

## See also

- [Architecture](.claude/architecture.md) — project layout, entity pattern,
  auth flow
- [New Entity Checklist](.claude/new-entity.md) — all steps to add a new
  entity type
- [stowaway](.claude/stowaway.md) — storage abstraction library (local dep)
- [app-lib](.claude/app-lib.md) — utility library (local dep)
- [Code Style](.claude/instructions.md) — naming, conventions, REPL workflow
- [Patterns](.claude/patterns.md) — cross-backend query patterns, solved
  problems
