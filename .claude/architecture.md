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

## Integrant System

`clj-money.system` defines the server's components (storage, image storage,
progress tracker factory, external service config, web handler and web
server) as an Integrant system. Storage-like components reach the code
through dynamic vars bound per request, by `system/with-components` and by the
test harness. The service config is passed explicitly: request handlers get
it with `clj-money.web.system/component`. See the "Integrant System" wiki page (https://git.dgknght.com/dgknght/clj-money/wiki/Integrant-System).

## Dual-Storage Model

SQL and Datomic are both supported simultaneously. Tests run against both via
the `dbtest` macro, which iterates over all configured strategies. API tests
use the `reset-db` fixture and run against the active strategy only. Both
reuse one Integrant-managed storage instance per test database (see the
"Testing" wiki page, https://git.dgknght.com/dgknght/clj-money/wiki/Testing).

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

## Receipt Ingestion (`ingestion/`)

A photo of a receipt is read by a vision model and turned into a transaction.
- `ingestion.clj` - the `Reader` protocol; the provider is chosen by config
  (`:ingestion` in `env/dev/config.edn`)
- `ingestion/ollama.clj` - the Ollama provider. `request-body` and `generate`
  are public so the evaluation harness sends exactly what the app sends
- `ingestion/receipts.clj` - the prompt, the JSON schema (`schema` looks up
  the entity's accounts; `build-schema` takes the account names directly), and
  `make-trx`, which builds the transaction from the model's answer. Whatever
  the items don't cover (tax the model didn't attribute to items, a tip) is
  spread across them in proportion to their amounts, so the transaction
  balances. Amounts are parsed from the model's JSON as decimals
- `ingestion/evaluation.clj` - a harness that scores models, with and without
  the GPU, against receipts with hand-written expected answers
  (`lein eval-receipts -- --help`). It calls a live Ollama server, so it is
  not part of the test suite and is excluded from coverage; its scoring
  functions are unit tested

Reading a receipt is slow, so it runs in the background, tracked by a
`receipt-ingestion` entity (`:status` is `:pending`, `:processing`,
`:complete` or `:failed`).
- `api/receipt_ingestions.clj` - `POST /api/entities/:entity-id/receipt-ingestions`
  takes the image (multipart, `image`), creates the receipt-ingestion and
  reads the receipt in a `future` with the `:ingestion` component. When
  complete, `:receipt-ingestion/receipt` holds the result in the receipt
  form's `:receipt/...` shape (accounts as refs), stored as edn.
  `GET /api/receipt-ingestions/:id` is polled by the client
- A successful read also creates the transaction, with
  `:transaction/source :ingestion` and `:transaction/review-status :pending`,
  referenced by `:receipt-ingestion/transaction`, and attaches the receipt
  image to it (caption "Receipt"). A transaction without a
  source was created by the user; it has no review status, or `:accepted`.
  The user accepts by updating the transaction's review status, and rejects
  with `PATCH /api/receipt-ingestions/:id` (`:status :rejected` and a required
  `:rejection-reason`), which deletes the transaction
- `api/receipt_ingestions.cljs` - the client functions
- `views/receipts.cljs` - choosing an image with the Scan button uploads it
  and polls until it's read, then opens the transaction in the form. Until
  then (`:reading?` in the page state), the form is replaced by placeholders
  and a spinner, and the receipt image replaces the recent transactions. While
  the form is unchanged, the buttons are Accept (saves with review status
  `:accepted`) and Reject (asks for a reason). Any edit restores Enter and
  Cancel, and saving still accepts it. Until it's accepted or rejected, the image
  input stays hidden and the receipt image stays up
- `views/ingestion_settings.cljs` - the off-canvas drawer, opened by the gear
  joined to the Scan button, for the entity settings the reader uses:
  `:settings/payment-methods` and `:settings/expense-accounts` (sets of
  account refs; Datomic retracts removed ones in `deconstruct`) and
  `:settings/expense-hints` (edited one per line)
- API tests pass their own reader to `web.test-handler/build-app`
