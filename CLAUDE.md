# CLAUDE.md

This application is a web UI over a double-entry accounting system.

## Tech Stack

- **Language:** Clojure 1.12 (backend), ClojureScript (frontend)
- **Web:** Ring + Reitit
- **Databases:** PostgreSQL (via next.jdbc), Datomic (peer and client modes)
- **Frontend:** Reagent + React 18
- **Build:** Leiningen

## Commands

- `lein repl` - open a repl, then `(go)`, `(reset)` and `(halt)` to manage
  the Integrant system (see the "Integrant System" wiki page)
- `lein test` - Run the full test suite in serial mode
- `bin/parallel-test -n 4` - Run the full test suite in parallel processes,
  each with its own database (faster; see the "Testing" wiki page,
  https://git.dgknght.com/dgknght/clj-money/wiki/Testing)
- `clj-kondo --lint src:test` - Run the linter
- `lein fig:build` - Build the client app and start a repl
- `lein fig:test` - Run the client tests (don't execuite if a client repl is active)

NEVER RUN THE TEST SUITE AGAINST THE DEVELOPMENT DATABASE.

When tests or the build fail unexpectedly, check `.claude/troubleshooting.md`
for known problems before investigating.

## Libraries

We own two of the libraries used throughout this projects.

- `dgknght.app-lib` - Source at `../app-lib`
- `stowaway` - Source at `../stowaway`

When a feature or a bug fix requires a changes to one of these libraries,
create an issue in their forgejo repository rather than making the change
directly.

## Guidelines

- After making changes:
  - Review the documentation and ensure that it is up-to-date. User-facing
    documentation lives in the Forgejo wiki
    (https://git.dgknght.com/dgknght/clj-money/wiki), documentation for Claude lives
    in `.claude/`, and README files stay with the code.
  - Run unit tests with code coverage to ensure all tests pass
    and coverage has not slipped below the configured minimum.
- Use [Conventional Commit](https://www.conventionalcommits.org/) messages
