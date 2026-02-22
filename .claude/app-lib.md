# dgknght/app-lib

Cross-platform (Clojure/ClojureScript) utility library used throughout
clj-money for web responses, authorization, validation, and UI components.

## Clojure Namespaces

### `dgknght.app-lib.api`
HTTP response helpers for Ring handlers.

**Response builders:**
- `response` — generic 200 response
- `creation-response` — 201 with Location header
- `update-response` — 200 with updated resource
- `no-content` — 204

**Pre-built error responses:**
- `not-found` — 404
- `forbidden` — 403
- `bad-request` — 400
- `unauthorized` — 401

**Middleware:**
- `wrap-authentication` — validates auth token, sets `:authenticated-user`
- `wrap-api-exception` — catches exceptions, returns appropriate HTTP response

---

### `dgknght.app-lib.authorization`
Multimethod-based authorization.

**Multimethods (dispatch on `[model-type action]`):**
- `allowed?` — returns truthy if the action is permitted
- `scope` — returns criteria to restrict queries to authorized records

**Helpers:**
- `authorize` — calls `allowed?`; throws if denied
- `+scope` — applies `scope` to a criteria map

**Actions** (all derive from `::manage`):
`::create`, `::show`, `::update`, `::destroy`, `::index`

---

### `dgknght.app-lib.validation`
Model validation.

**Core:**
- `validate` — runs validators against a model; returns model with errors
- `has-error?` — check if a model has any errors
- `valid?` — true if no errors
- `error-messages` — nested error map
- `flat-error-messages` — flattened error sequence

**Macros:**
- `with-validation` — validate and branch on valid/invalid
- `with-ex-validation` — validate; throw on invalid

**Built-in predicates:**
- `email?`, `local-date?`, `non-empty-string?`
- `positive-integer?`, `positive-big-dec?`, `nilable-local-date?`

---

### `dgknght.app-lib.core` (cljc)
General-purpose utilities, available on both Clojure and ClojureScript.

| Function | Description |
|----------|-------------|
| `update-in-if` | `update-in` only when key exists |
| `assoc-if` | `assoc` only when value is truthy |
| `assoc-unless` | `assoc` only when value is falsy |
| `index-by` | build a map keyed by a function of each element |
| `present?` | true if value is non-nil and non-empty |
| `presence` | return value if present, else nil |
| `uuid` | generate a random UUID |
| `parse-int` | parse string to integer |
| `parse-bool` | parse string to boolean |
| `parse-decimal` | parse string to `BigDecimal` |
| `fmin` / `fmax` | find min/max of a collection |
| `->sequential` | wrap non-sequential values in a vector |
| `dissoc-in` | remove a nested key |
| `deep-contains?` | check nested path existence |
| `deep-get` | get nested value |
| `deep-update-in-if` | update nested value only when path exists |
| `prune-to` | remove nil/empty values recursively |

---

### `dgknght.app-lib.models`
Model map utilities.

| Function | Description |
|----------|-------------|
| `->id` | extract `:id` from a model or return the value if already an id |
| `map-index` | build an id-keyed map from a sequence of models |
| `nest` | convert flat map with prefixed keys to nested map |
| `unnest` | inverse of `nest` |
| `extract-nested` | pull out a nested sub-map |

---

### `dgknght.app-lib.inflection` (cljc)
String inflection utilities.

| Function | Description |
|----------|-------------|
| `humanize` | convert `:kebab-case` keyword to "Human Readable" string |
| `title-case` | capitalize each word |
| `singular` | convert plural to singular form |
| `conjoin` | join with commas and "and" |

---

### `dgknght.app-lib.web` (Clojure)
- `format-date` — format `LocalDate` for display
- `format-decimal` — format `BigDecimal` for display
- `format-percent` — format as percentage
- `path` — construct URL paths

---

### `dgknght.app-lib.math` (cljc)
Financial/decimal arithmetic utilities.

---

## ClojureScript Namespaces

### `dgknght.app-lib.forms`
Reagent form field components:
- `text-field` — text input
- `select-field` — dropdown select
- `checkbox-field` — checkbox input
- `defaults` — global form defaults (e.g. Bootstrap framework)

### `dgknght.app-lib.api-3`
ClojureScript HTTP client:
- `get`, `post`, `put`, `delete` — HTTP methods
- `path` — build API paths
- Defaults to EDN encoding/decoding

### `dgknght.app-lib.forms-validation`
Client-side validation:
- `validate`, `valid?`
- Built-in rules: `::v/required`, `::v/email`

### `dgknght.app-lib.dom`
- `set-focus` — programmatically focus a DOM element

### `dgknght.app-lib.bootstrap-5`
Bootstrap 5 Reagent component wrappers (modals, alerts, etc.)

### `dgknght.app-lib.html`
General HTML helper components.

### `dgknght.app-lib.notifications`
User feedback utilities:
- `dangerf` — display error notification (format string)
- Variants for success, warning, info

### `dgknght.app-lib.calendar`
Date picker Reagent components.

### `dgknght.app-lib.decimal`
Decimal number display utilities.

---

## Test Utilities

### `dgknght.app-lib.test`
Test helpers for Ring handler tests:
- `parse-json-body` — parse response body as JSON
- `parse-edn-body` — parse response body as EDN
- `parse-html-body` — parse response body as HTML
- `multipart-body` — build a multipart request body
- `with-mail-capture` — capture outbound email in tests
- `with-log-capture` — capture log output in tests
- `attr-set` — extract a set of attribute values from a sequence of maps

### `dgknght.app-lib.test-assertions`
Custom `clojure.test` assertions:

**HTTP status:**
`http-success?`, `http-created?`, `http-no-content?`, `http-not-found?`,
`http-forbidden?`, `http-unauthorized?`, `http-bad-request?`,
`http-unprocessable?`

**Sequence assertions:**
`seq-of-maps-like?`, `seq-with-map-like?`, `seq-with-no-map-like?`,
`seq-containing-model?`, `seq-excluding-model?`

**Model assertions:**
`conformant?`, `valid?`, `invalid?`, `comparable?`, `thrown-with-ex-data?`,
`same-date?`
