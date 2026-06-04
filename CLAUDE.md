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

## Guidelines

- After making changes:
  - Review the documentation and ensure that it is up-to-date.
  - Run unit tests with code coverage to ensure all tests pass
    and coverage has not slipped below the configured minimum.
