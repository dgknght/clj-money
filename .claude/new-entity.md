# New Entity Checklist

When adding a new entity type, touch ALL of the following. Do not skip steps.

## Steps

### 1. SQL Migration
Create `resources/migrations/YYYYMMDDNNNNNN_add_<entity>.up.sql` and
`...down.sql`. Define the table and any indexes in the up file; drop them in
the down file.

### 2. Spec Definitions
Create `src/clj_money/entities/<entity>s.clj` with `clojure.spec` definitions
for the entity and any business rule validation.

### 3. SQL `after-read`
Create `src/clj_money/db/sql/<entity>s.clj`. Implement `after-read` to coerce
date columns from Java types to `java.time.LocalDate`. Also implement
`entity-keys` and `before-save` as needed.

### 4. Authorization
Create `src/clj_money/authorization/<entity>s.clj`. Implement the `allowed?`
multimethod (for `::create`, `::show`, `::update`, `::destroy`, `::index`)
and the `scope` multimethod.

### 5. API Server Handler
Create `src/clj_money/api/<entity>s.clj` with Ring handler functions and
route definitions.

### 6. API ClojureScript Client
Create `src/clj_money/api/<entity>s.cljs` with the ClojureScript API client
(HTTP calls, response parsing).

### 7. Datomic Schema
Create `resources/datomic/schema/<entity>.edn` with the Datomic attribute
definitions for the entity.

### 8. Datomic `after-read`
Create `src/clj_money/db/datomic/<entity>s.clj`. Implement `after-read` for
type coercion (Datomic returns `java.util.Date`; coerce to `LocalDate`). Also
implement `deconstruct` if the entity has refs or composite structure.

### 9. Entity Schema Registry
Add an entry for the new entity to the `entities` vector in
`src/clj_money/entities/schema.cljc`. Include fields, refs, and primary key.

### 10. Entity `ref.clj`
Require the new entity namespace in `src/clj_money/entities/ref.clj` to
trigger multimethod registration.

### 11. SQL `ref.clj`
Require the new SQL namespace in `src/clj_money/db/sql/ref.clj`.

### 12. Datomic `ref.clj`
Require the new Datomic namespace in `src/clj_money/db/datomic/ref.clj`.

### 13. Datomic Schema Task
Add the schema filename to the list in the `schema` function in
`src/clj_money/db/datomic/tasks.clj`.

### 14. `bounding-where-clause`
Add a case for the new entity in `bounding-where-clause` in
`src/clj_money/db/datomic.clj`.

### 15. Auth Helpers (`fetch-entity`)
Add a `fetch-entity` dispatch in `src/clj_money/entities/auth_helpers.clj`
for the new entity if it requires `owner-or-granted?`. For multi-hop chains
(e.g. memo-ledger-entry → lot → entity), call `fetch-entity` recursively.

### 16. Server Routes
In `src/clj_money/web/server.clj`, require the new API namespace and add its
routes to the router.

### 17. Test Context
In `test/clj_money/test_context.clj`:
- Add a `find-<entity>` helper function
- Add a `prepare` multimethod dispatch for seeding the entity in test fixtures

### 18. Apply Migration
Run `lein with-profile test migrate` to apply the new migration to the test
database.

## Quick Reference

```
resources/migrations/YYYYMMDDNNNNNN_add_<entity>.{up,down}.sql
src/clj_money/entities/<entity>s.clj
src/clj_money/db/sql/<entity>s.clj
src/clj_money/authorization/<entity>s.clj
src/clj_money/api/<entity>s.clj
src/clj_money/api/<entity>s.cljs
resources/datomic/schema/<entity>.edn
src/clj_money/db/datomic/<entity>s.clj
src/clj_money/entities/schema.cljc       ← add to entities vector
src/clj_money/entities/ref.clj           ← require new ns
src/clj_money/db/sql/ref.clj             ← require new ns
src/clj_money/db/datomic/ref.clj         ← require new ns
src/clj_money/db/datomic/tasks.clj       ← add schema filename
src/clj_money/db/datomic.clj             ← bounding-where-clause case
src/clj_money/entities/auth_helpers.clj  ← fetch-entity dispatch
src/clj_money/web/server.clj             ← require + routes
test/clj_money/test_context.clj          ← find-<entity> + prepare
```
