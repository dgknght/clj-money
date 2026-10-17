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

`clj-money.system` defines the server's components (storage, the change
publisher and propagation subscription, the budget monitor cache, image storage,
progress tracker factory, external service config, web handler and web
server) as an Integrant system. Storage-like components reach the code
through dynamic vars bound per request, by `system/with-components` and by the
test harness. The service config is passed explicitly: request handlers get
it with `clj-money.web.system/component`. See the "Integrant System" wiki page (https://git.dgknght.com/dgknght/clj-money/wiki/Integrant-System).

## Propagation (publish/subscribe)

Changes to one entity often affect others (a transaction changes account
balances, a price changes account values, a lot changes a commodity's shares
owned). This follows the publish/subscribe pattern used in clj-groceries:

- `entities/put-many` and `entities/delete-many` publish a `[before after]`
  change tuple for each changed entity to the `::entities/publisher`
  component, bound per request as `entities/*publisher*` (or passed with the
  `:publisher` option; `nil` publishes nothing). When no publisher is bound
  (CLI tasks, the REPL, imports), nothing is published.
- The `::prop/subscription` component (`entities/propagation.clj`) subscribes
  to every entity type that implements the `prop/propagate` multimethod and
  propagates the changes **asynchronously**, one at a time, in publish order,
  on a background thread. `propagate` returns the entities to put; those puts
  are not themselves published. After handling each change (even when it
  fails), it signals `::prop/propagated` with the change under
  `::prop/change`, for subscribers that depend on the propagated state.
- The publisher taps a single `core.async` mult rather than using `pub`, so
  that a subscriber sees messages in publish order across topics.
- Because propagation is asynchronous, a transaction can be propagated before
  an earlier transaction in the same account has been. Re-indexing takes its
  basis from the last *indexed* item, and propagating the earlier
  transaction re-indexes everything after it.
- Imports and test contexts bind `*publisher*` to nil and run
  `prop/propagate-all` once at the end instead.
- The subscription exports metrics: `clj_money.propagation.duration` and
  `clj_money.propagation.lag` (publish to propagated; `publish*` stamps
  each message with `::entities/published-at` metadata) histograms, tagged
  with `entity.type` and, on failure, `error.type`; and the
  `clj_money.propagation.queue.size` gauge, tagged with `queue`
  (`publisher` or `subscription`). Each channel buffers 1000 messages and
  core.async throws on `put!` once 1024 more are pending, so a publisher
  queue size approaching ~2000 means requests are about to fail.

In tests, `with-test-storage` (used by `dbtest` and `reset-db`) starts a
publisher and subscription for each test. Use `put-and-propagate`,
`delete-and-propagate`, `propagated` or `await-propagation` from
`clj-money.test-helpers` before asserting on propagated state. The test web
handler (`web/test_handler.clj`) waits for propagation after each request.

## Budget Monitor Cache (`monitor_cache.clj`)

The dashboard's budget monitors (`reports/monitor`) are slow to compute, so
`GET /api/entities/:entity-id/reports/budget-monitors` gets them through the
`::monitor-cache/cache` component (`monitor-cache/fetch`; with no cache, as
in the REPL and most tests, they're computed every time). Monitors are
cached by account and as-of date, and evicted on a background thread:

- for a transaction, on its `::prop/propagated` signal, not on the change
  itself, because monitors are computed from propagated balances. Only the
  monitors whose account or descendants (`reports/monitored-account-ids`)
  appear on its items, before or after the change, are evicted.
- for any budget, budget item or account change, all monitors.
- after `:ttl-minutes` (config `:monitor-cache`, default 60), for changes
  that aren't published (imports, the REPL).

Every eviction advances a generation counter, and a monitor computed while
the generation changed isn't stored. Use `monitor-cache/await-evictions` in
tests before asserting on a cached monitor.

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
- `ingestion.clj` - the `ReceiptReader`, `StatementReader` and `Closeable`
  protocols, and the `receipt-reader` and `statement-reader` multimethods,
  which build a provider's reader from config (`:ingestion` in
  `env/dev/config.edn`). `ingestion/reader.clj` defines a component for each,
  `::ing/receipt-reader` and `::ing/statement-reader` (the handler's
  `:receipt-reader` and `:statement-reader`), both given the same config: one
  reader for every kind of document, or, when the config has `:receipts` or
  `:statements`, a separate reader for that kind (e.g., Gemini for receipts
  and a local Ollama model for the more sensitive statements). A reader under
  one of those keys takes nothing from the rest of the config. Readers are
  composed in the config: a decorator is a provider too, whose method builds
  the reader under its `:reader` and wraps it. `::otel-ing/observable`
  (`otel/ingestion.clj`, receipts and statements) traces and measures calls,
  attributed to the innermost reader's provider and model; `::pdf/pdf`
  (statements only) is described below. Unwrapped readers aren't measured.
  `ingestion/ref.clj` requires every provider's namespace so the methods load
- Ollama models are given a receipt's contents rather than the image, by one
  of two receipt decorators (issue #208): `::ocr/ocr` (`ingestion/ocr.clj`)
  runs the `tesseract` command (installed in the Dockerfile; `:command`,
  `:language` and `:psm` are configurable) and gives `{:text ...}`, read
  much faster by a text model (`qwen3:8b` by default); a PDF's own text is
  used when it has some. `::image/image` (`ingestion/image.clj`, which holds
  `scale-image`) gives `{:images [base64 ...]}`, for the `:vision-model`, as
  the app did before. An undecorated Ollama receipt reader still accepts the
  image, with a warning. `ollama/receipt-defaults` holds the models
- `ingestion/ollama.clj` - the Ollama provider. `request-body` and `generate`
  are public so the evaluation harness sends exactly what the app sends
- `ingestion/receipts.clj` - the prompt, the JSON schema (`account-options`
  looks up the entity's accounts; `schema` takes those options, and each
  reader passes the `:account-options` it's given), and
  `make-trx`, which builds the transaction from the model's answer. Whatever
  the items don't cover (tax the model didn't attribute to items, a tip) is
  spread across them in proportion to their amounts, so the transaction
  balances. Amounts are parsed from the model's JSON as decimals. An account
  the model didn't choose, or that doesn't exist, is replaced with the first
  of its choices (payment methods by name, expense accounts by path) for the
  user to correct; with no payment methods set up, the read fails
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
  reads the receipt in a `future` with the `:receipt-reader` component. When
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

## Statement Ingestion (`ingestion/statements.clj`)

A PDF of a bank or credit card statement is read by the model and turned into
an unfinished (`:new`) reconciliation of the account (issue #182).
- `StatementReader/read-statement` - Gemini reads the PDF itself, and allows a
  statement more output tokens than a receipt. Ollama models can't read a PDF,
  so its config is wrapped in a `::pdf/pdf` reader (`ingestion/pdf.clj`,
  PDFBox; neither knows of the other), which gives it the PDF's contents in place of the PDF: `{:text ...}`, sent
  with the prompt to a text model (`qwen3:8b` by default), or, for a PDF with
  too little text to have been anything but scanned, `{:page-images [...]}`
  for the `:vision-model`. `ollama/statement-defaults` holds the larger
  context and output limits a statement needs
- `ingestion/statements.clj`:
  - `account-options`: the asset and liability paths the account is chosen
    from (left out when the user gave the account), and the leaf paths of
    every non-equity account for the other side of each line
  - `schema` and `prompt`: each line's `amount` is **signed by its effect on
    the account balance**, so a deposit to checking and a charge on a credit
    card are both positive. This matches `:transaction-item/polarized-quantity`
  - `candidate-items`: the account's items from 7 days before the period
    through its end that aren't in a completed reconciliation
  - `match-items`: a pure function pairing each line with an item of the same
    polarized amount within 7 days. The nearest date wins, then the most
    similar description (`ingestion/text.clj`). Each item is used once
  - `make-trx`: a `:source :ingestion`, `:review-status :pending` transaction
    for each unmatched line. The model's category is used, or the first income
    (money in) or expense (money out) account
  - `make-reconciliation`: the matched and new items, the statement's closing
    date and balance. Adds to the account's reconciliation in progress, if any
- `api/statement_ingestions.clj` - `POST /api/entities/:entity-id/statement-ingestions`
  (multipart `statement`, optional `account-id`) and `GET
  /api/statement-ingestions/:id`, like receipts. It reads the statement in a
  `future` with the `:statement-reader` component
- `ingestion/statement_reconciliation.clj` - `reconcile` does the work: reads
  the statement, resolves the account, matches the lines, creates the
  transactions and saves the reconciliation. The `statement-ingestion` entity
  holds the model's raw answer and any `:warnings` (e.g., the opening balance
  doesn't match the last reconciliation), both as edn. A statement ending on
  or before the last completed reconciliation fails before anything is saved;
  if the reconciliation can't be saved, the new transactions are deleted
- The reconciliation refers to the statement's image
  (`:reconciliation/statement`), set by the read or attached by hand from the
  reconciliation form (uploaded with `POST /api/images`). The ref is safe for
  the join planner because the path from reconciliation to user through the
  image is longer than the one through the account. The account page's
  Reconciliations button lists them, latest first, with each statement
- An image is shared with an entity by what refers to it, an attachment or a
  reconciliation's statement (`authorization/images.clj`), and a grant's
  permission for that kind of thing covers the image. `web/images.clj` finds
  the image by id and leaves the decision to the authorization. Note that
  grants don't match at all yet: `allowed?` gets namespaced actions
  (`::authorization/show`) and a grant's permissions hold plain ones (`:show`)
- The statement-ingestion refers to its account but **not** to the
  reconciliation: the ref would give stowaway's join planner a second path
  from reconciliation to entity, as short as the one through the account, and
  scoped reconciliation queries took it and came back empty. Find the
  reconciliation as the account's `:new` one
- Completing a reconciliation (`api/reconciliations.clj`) accepts its pending
  transactions; saving one from the transaction form accepts it too
- `views/statement_upload.cljs` - the file input and poll loop, shared by
  `views/statements.cljs` (the `/statements` page, which hands the account to
  the accounts page via `:pending-reconciliation` in `app-state`) and the
  "Read from a statement" input on the reconciliation form
- Test fixtures (`fixtures/statements/`) are fictitious statements
  with the expected model answers; real statements must not be committed
- API tests pass their own reader to `web.test-handler/build-app`
