# clj-money Architecture

## Tech Stack
- **Language:** Clojure 1.12 (backend), ClojureScript (frontend)
- **Web:** Ring + Reitit
- **Databases:** PostgreSQL (via next.jdbc), Datomic (peer and client modes)
- **Frontend:** Reagent + React 18
- **Build:** Leiningen

## Directory Layout

```
src/clj_money/
  api/           # Server-side route handlers (.clj) + ClojureScript clients (.cljs)
  authorization/ # allowed? and scope multimethods per entity
  db/
    sql/         # SQL multimethods: entity-keys, before-save, after-read
    datomic/     # Datomic multimethods: deconstruct, after-read
  entities/      # clojure.spec definitions + business rules
  import/        # Data import logic
  prices/        # Price fetching
  views/         # ClojureScript Reagent views
  web/           # Ring server, middleware, routing

resources/
  migrations/          # Flyway SQL migration files
  datomic/schema/      # Datomic schema EDN files

test/clj_money/        # Mirrors src structure, _test suffix
```

## Dual-Storage Model

SQL and Datomic are both supported simultaneously. Tests run against both via
the `dbtest` macro, which iterates over all configured strategies. API tests
use the `reset-db` fixture and run against the default strategy only.

## Entity Attribute Naming

All entity attributes use namespaced keywords: `:account/name`,
`:transaction/date`. This convention is consistent across SQL and Datomic.

## 4-Layer Entity Pattern

Each entity type has implementations across four layers:

| Layer | File | Responsibilities |
|-------|------|-----------------|
| Spec | `entities/<entity>s.clj` | `clojure.spec` definitions, business rules |
| SQL | `db/sql/<entity>s.clj` | `entity-keys`, `before-save`, `after-read` |
| Datomic | `db/datomic/<entity>s.clj` | `deconstruct`, `after-read` |
| Auth | `authorization/<entity>s.clj` | `allowed?`, `scope` |
| API | `api/<entity>s.clj` + `.cljs` | server handlers, ClojureScript client |

## Central Entity Registry (`entities/schema.cljc`)

Defines all 21 entity types with their fields, refs, and primary keys.
Drives `attributes`, `reference-attributes`, `relationships`, and `prune`
functions used throughout the codebase.

## Authorization Flow

`entities/auth_helpers.clj` defines the `fetch-entity` multimethod, which
resolves authorization chains — e.g. attachment → transaction → entity → user.
`owner-or-granted?` performs the final ownership/grant check.

Multi-hop chains call `fetch-entity` recursively. Add a dispatch for any new
entity that requires `owner-or-granted?`.

## `bounding-where-clause` (db/datomic.clj)

Needs a case for every entity type. Add one when creating a new entity.

## ref.clj Files

Each storage layer has a `ref.clj` that requires all entity-specific namespaces
to trigger multimethod registration:
- `entities/ref.clj`
- `db/sql/ref.clj`
- `db/datomic/ref.clj`

## Test Infrastructure (`test/clj_money/test_context.clj`)

Provides:
- `find-<entity>` helper functions for locating test records
- `prepare` multimethod for seeding entity data
- `basic-context` fixture used across test namespaces
