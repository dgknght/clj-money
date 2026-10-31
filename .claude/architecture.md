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

## Price Providers (`prices/`)

`clj-money.prices/Provider` fetches the price of a commodity for a date
(`(fetch provider commodity {:date d})`, nil when there's none). The
`::prices/provider` component is built from `:prices` by the
`prices/provider` multimethod, and request handlers get it with
`(clj-money.web.system/component req :prices)`. Providers are composed in the
config: a decorator's method builds the provider under its `:inner` key and
wraps it.
- `::alpha-vantage/provider`, `::eodhd/provider` call the external APIs.
  Both quote `:currency` commodities (crypto) in USD: Alpha Vantage with
  `DIGITAL_CURRENCY_DAILY`, eodhd with the `<SYM>-USD.CC` ticker. The fetch
  API never asks for the price of the entity's default commodity
- `::redis/cache` caches fetched prices by symbol and date. `:bypass-cache?
  true` in the fetch options ignores the cached price and replaces it with a
  fresh one (the API's `bypass-cache=true` param, the "Bypass cache" checkbox
  in the Prices card)
- `::fallback/provider` tries each of its `:providers` in order and returns
  the first price found. A provider that throws is treated as having no
  price, so the next one is tried; nil when none has a price. The fallback
  logs only a one-line warning; wrap each provider in `::observable/provider`
  for the stack trace and error metrics
- `::observable/provider` logs and traces each fetch (`prices/fetch` span) and
  records `clj_money.prices.fetch.duration`, attributed to the inner provider
  and exchange, with an outcome of `found`, `not_found` or `error` (plus
  `error.type`). The provider is identified by the observable's `:name` (e.g.
  `"alpha-vantage"`), or by the inner `::prices/provider` key when it's
  omitted, in the metric and span attributes and as a `[name]` prefix on its
  log entries. Put it inside the cache so only external calls are measured,
  and wrap each provider separately to tell them apart
- Tests use `clj-money.prices.mock/mock` (test config) or pass their own
  provider to `test-handler/build-app` as `:prices`

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

The dashboard's budget monitors (`reports.monitor/report`) are slow to
compute, so `GET /api/entities/:entity-id/reports/budget-monitors` gets them
through the `::monitor-cache/cache` component (`monitor-cache/fetch`; with
no cache, as in the REPL and most tests, they're computed every time).
Monitors are cached by account and as-of date, and evicted on a background
thread:

- for a transaction, on its `::prop/propagated` signal, not on the change
  itself, because monitors are computed from propagated balances. Only the
  monitors whose account or descendants (`reports.monitor/account-ids`)
  appear on its items, before or after the change, are evicted.
- for any budget, budget item or account change, all monitors.
- after `:ttl-minutes` (config `:monitor-cache`, default 60), for changes
  that aren't published (imports, the REPL).

Every eviction advances a generation counter, and a monitor computed while
the generation changed isn't stored. Use `monitor-cache/await-evictions` in
tests before asserting on a cached monitor.

## Reports (`reports.clj`, `reports/`)

Each report lives in its own child namespace of `clj-money.reports`
(`balance-sheet`, `income-statement`, `budget`, `monitor`, `portfolio`,
`lots`, `commodities-account-summary`), and each exposes a `report`
function. Code shared by more than one report, such as
`valuate-accounts` and `summarize-account-type`, stays in
`clj-money.reports`.

## Report Cache (`report_cache.clj`)

The balance sheet, income statement, and budget reports are fetched through
the `::report-cache/cache` component (`report-cache/balance-sheet`,
`income-statement`, `budget`; with no cache they're computed every time).
It works like the monitor cache, with the same generation counter, TTL
(config `:report-cache`, default 60 minutes) and `await-evictions`. Each
report is cached by its entity or budget and dates, and covers a range of
transaction dates (a balance sheet: up to its as-of date; an income
statement: since to as-of; a budget report: the budget's start date to its
as-of date, or end date when none is given). Reports are evicted:

- for a transaction, on its `::prop/propagated` signal, only the reports of
  its entity whose range includes its transaction date, before or after the
  change.
- for a propagated price or lot change, all reports.
- for any account, budget, budget item or entity change, all reports.

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

`allowed?` gets namespaced actions (`::authorization/show`), but a grant's
permissions hold plain ones (`:show`, see `grants/actions`).
`user-granted-access?` maps one to the other; `::destroy` becomes `:delete`
and `::recalculate` becomes `:update`. An action missing from that map is
never granted. Resource types that aren't in `grants/resource-types` take
the permissions of the type they belong to (`grant-resource-types`, e.g. a
lot or trade takes `:account`'s, and a transaction item or scheduled
transaction takes `:transaction`'s).

`scope` takes an action. With `::authorization/index`, the default for
`+scope`, it limits a query to the entities the user owns, plus those the
user holds an `:index` grant on for the resource type. Lists use this.
With `::authorization/manage`, any permission on the type will do. Use it
for a lookup by id that goes on to `authorize`, which then checks the action
itself. Never pass `::manage` to a lookup that skips `authorize`. Each
`scope` method calls `auth-helpers/entity-scope` with the attribute that
reaches the entity, e.g. `:transaction/entity` for an attachment, which
joins through the transaction. A user without any entities gets the old
owner-only criteria, because SQL can't query `IN ()`.

`authorize` throws `::authorization/not-found` when the user can't see the
resource, and `::authorization/forbidden` when the user can see it but not
take the action. `middleware/handle-exception` returns 404 and 403 for these.

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
  `:language` and `:psm` are configurable) with Sauvola thresholding, killing
  it after `:timeout-ms` (60000, for each image; issue #221, via
  `clj-money.shell/sh`, which is `clojure.java.shell/sh` with a timeout),
  first
  enlarging an image smaller than `:min-image-dimension` (3000, via
  `scale/enlarge`), and gives `{:text ...}`, read
  much faster by a text model (`qwen3:8b` by default); a PDF's own text is
  used when it has some. `ocr` returns nil when no text is read, Tesseract
  times out, or the total the model read isn't among the amounts in the text (e.g., a
  handwritten total OCR misread), so configure it as the first of the
  `:alternatives` of a `::logic/or` reader (`ingestion/logic.clj`, issue
  #222), followed by `scale` → `base64`, so the slow vision model only reads
  what OCR can't. `::logic/or` reads the receipt into memory and tries each
  alternative in turn, returning the first non-nil result (and throwing if
  there's none); its `:reader` (e.g., `ollama`) is created once and put
  under the `:reader` of each alternative's innermost decorator.
  `::base64/base64` (`ingestion/base64.clj`) gives
  `{:images [base64 ...]}`, for the `:vision-model`, encoding the image as it
  is; configure it under `::scale/scale` (`ingestion/scale.clj`, which holds
  `scale-image`, `:max-image-dimension` 1280 by default) to send the image
  scaled down. `ollama.clj` knows nothing of images beyond sending the
  base64 it's given, and refuses a receipt or PDF itself. Configure
  `::orient/orient` (`ingestion/orient.clj`) outside either, to turn a
  phone photo upright as its EXIF orientation tag says (read from the JPEG
  header by hand, without an EXIF library); neither Tesseract nor `ImageIO`
  applies the tag, so a sideways photo reads as nothing.
  `ollama/receipt-defaults` holds the models
- `ingestion/ollama.clj` - the Ollama provider. `request-body` and `post-request`
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
  (PNG bytes). `::scale/scale` and `::base64/base64` also decorate statement
  readers, scaling the page images and turning them into the
  `{:images [base64 ...]}` the Ollama reader sends to the `:vision-model`;
  both pass `{:text ...}` on as it is. So the Ollama chain for statements is
  `pdf` → `scale` (`:max-image-dimension 1600`) → `image` → `ollama`.
  `ollama/statement-defaults` holds the larger context and output limits a
  statement needs
- `ingestion/statements.clj`:
  - `account-options`: the asset and liability paths the account is chosen
    from (left out when the user gave the account; commodity accounts are
    never offered), and the leaf paths of every non-equity account for the
    other side of each line. `:symbols` (the entity's non-currency commodity
    symbols) is added when the account is a trading account, or when it's
    unknown and the entity has one; it's what turns on the investment parts
    of the `schema` and `prompt`
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
    (money in) or expense (money out) account (`other-account`)
  - `make-reconciliation`: the matched and new items, the statement's closing
    date and balance. Adds to the account's reconciliation in progress, if any
- `api/statement_ingestions.clj` - `POST /api/entities/:entity-id/statement-ingestions`
  (multipart `statement`, optional `account-id`) and `GET
  /api/statement-ingestions/:id`, like receipts. It reads the statement in a
  `future` with the `:statement-reader` component. `PATCH
  /api/statement-ingestions/:id` corrects a `:failed` or `:conflict`
  ingestion whose statement was read (422 otherwise): it merges the
  `:period-start`, `:period-end`, `:opening-balance` and `:closing-balance`
  given in `:statement-ingestion/statement` into the stored statement (blank
  ones keep what the model read), optionally sets
  `:statement-ingestion/account`, and runs `reconcile-statement` again in a
  `future`, without the model, replacing the reconciliation in progress if
  `:statement-ingestion/replace?` is true
- `ingestion/statement_reconciliation.clj` - `reconcile` reads the statement
  and saves it on the ingestion, then `reconcile-statement` does the rest,
  so that a statement that can't be reconciled keeps what the model read, for
  the user to correct (`views/statement_upload.cljs` shows it in a form in
  place of the reading indicator; on the reconciliation form, it fills the
  form's end of period and balance instead, and Try Again sends those):
  resolves the account, matches the lines, creates the
  transactions and saves the reconciliation (or, for a trading account, the
  reconciliations; see below). The `statement-ingestion` entity
  holds the model's raw answer and any `:warnings` (e.g., the opening balance
  doesn't match the last reconciliation), both as edn. A statement ending on
  or before the last completed reconciliation fails before anything is saved.
  So does one whose account has a reconciliation in progress with a different
  end of period or balance, with status `:conflict`, until the user confirms
  that it should be replaced (`replace?`). Replacing removes the items from
  the reconciliations in progress first, before any transaction is created:
  propagating a new transaction re-puts the later items in the account as
  they were read, which put back an item removed from the reconciliation in
  the meantime;
  if the reconciliation can't be saved, the new transactions are deleted
  with `trading/delete-transaction`, which undoes a trade (restoring its
  lots), latest first. The statement is attached to the new transactions only
  once everything else is saved: the background propagation of an
  attachment re-puts its transaction's `:transaction/attachment-count`, which
  in Datomic resurrected a transaction deleted by the rollback
- The reconciliation refers to the statement's image
  (`:reconciliation/statement`), set by the read or attached by hand from the
  reconciliation form (uploaded with `POST /api/images`). The ref is safe for
  the join planner because the path from reconciliation to user through the
  image is longer than the one through the account. The account page's
  Reconciliations button lists them, latest first, with each statement
- An image is shared with an entity by what refers to it, an attachment or a
  reconciliation's statement (`authorization/images.clj`), and a grant's
  permission for that kind of thing covers the image. `web/images.clj` finds
  the image by id and leaves the decision to the authorization
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

### Investment statements (`ingestion/investment_statements.clj`)

A brokerage or retirement account statement reconciles a trading account
(issue #211). The trading account holds the cash, in currency, and each
commodity account beneath it holds shares, so they can't share one balance:
- With `:symbols` in the account options, each line also has a `type`
  (`cash`, `buy`, `sell` or `reinvest`), and a trade has its `symbol` and
  `shares` (always positive; `normalize` takes the absolute value). The
  statement has `holdings`, the shares of each commodity at the end of the
  period. The opening and closing balances are of the cash only. A reinvested
  dividend is one `reinvest` line whose `amount` is the dividend
- `resolve-lines` finds each trade's commodity by symbol; a trade in a
  commodity the entity doesn't have is dropped, with a warning
- `match-lines`: a trade matches an item in its commodity account with the
  same change in shares within 7 days (`match-items` with `:match?`). Each
  matched trade's cash movement, dated as it was recorded, is then matched to
  the trading account's items (two for a reinvestment: the dividend in and
  the purchase out). Items have no reference to their transaction in
  Datomic, so they can't be paired that way. The cash lines are matched to
  the cash items left
- Unmatched trades are recorded with `trading/buy` (a reinvestment with
  `:trade/dividend?` and the model's category as the dividend account) or
  `trading/sell`, with `:trade/transaction-attributes` marking their
  transactions `:source :ingestion`, `:review-status :pending`. Unmatched
  cash lines use `make-trx`
- `make-reconciliations`: one `:new` reconciliation for the trading
  account's cash, with the closing balance, and one for each commodity
  account with items on the statement or shares in the holdings, with the
  shares held (zero if not in the holdings). Holdings of a commodity with no
  account under the trading account produce a warning. A commodity account
  with a completed reconciliation on or after the statement's end is left
  as it is, with a warning, rather than failing the statement
