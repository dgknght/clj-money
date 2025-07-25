# Project Overview: clj-money

This is a personal finance management application written in Clojure and
ClojureScript, intended as a web-based alternative to GnuCash. The backend
is pluggable. The current implementation is PosgtreSQL. A Datomic backend
is being developed. The frontend is built with reagent.

# Commands

*   **Run tests:** `lein test`
*   **Start dev server:** `lein figwheel`
*   **Lint files:** `clj-kondo --lint src`
*   **Build for production:** `lein uberjar`

# Conventions

*   Use kebab-case for all Clojure/ClojureScript functions and variables.
*   Commit messages should follow the Conventional Commits specification (e.g., `feat: ...`, `fix: ...`).
*   All database interactions should go through functions in the `clj-money.db` namespace. Do not write raw SQL in other parts of the application.
*   Frontend components should be placed in `src/clj_money/views/`.

# Key Files

*   `project.clj`: Project definition and dependencies.
*   `resources/migrations/`: Database schema migrations.

# Rules

*   **DO:** Always update or create a corresponding test file in `test/clj_money/` when adding new functionality to a `.cljc` or `.clj` file.
*   **DON'T:** Do not add new dependencies to `project.clj` without asking first.
