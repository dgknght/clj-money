# Patterns & Conventions

## Stowaway Criteria: Filtering by a Cardinality-Many Ref Attribute

### Problem
You need to filter entities by a cardinality-many ref attribute (e.g., return
only `lot-note`s that belong to a specific `lot`).

### Solution
Use a model-ref map as the criteria value directly:

```clojure
{:lot-note/lots {:id lot-id}}
```

This works cross-backend:
- **Datomic**: stowaway's `normalize-criterion` converts `{:id eid}` model-refs
  to scalar Long EIDs before building the Datalog where clause, then matches
  against the cardinality-many `:lot-note/lots` attribute.
- **SQL**: stowaway SQL's `normalize-model-ref` in `->clauses` handles the
  model-ref value via the join config
  `{[:lot :lot-note] [[:id [:any :lot-note/lot-ids]]]}`, which generates an
  `= ANY(lot_ids)` array-containment check for PostgreSQL integer[] columns.

### What Does NOT Work

| Approach | Why it Fails |
|---|---|
| `{:lot-note/lots [{:id lot-id}]}` | Plain vector without a `::predicate` keyword fails stowaway's `::criteria-value` spec |
| `{:lot-note/lots [:including {:id lot-id}]}` | Datomic `:including` handler passes the map as input to the ref attr; Datomic needs a raw Long EID — throws `Cannot resolve key` |
| `{:lot-note/lots [:including lot-id]}` | SQL `:including` generates a subquery (for nested criteria), not array containment — returns wrong results |
| In-memory filter after `entities/select` | Works but is wasteful; prefer DB-level filtering |

### API Handler Pattern
For a REST endpoint scoped under `lots/:lot-id/lot-notes`:

```clojure
(defn- index
  [{:keys [authenticated] {:keys [lot-id]} :params}]
  (api/response
    (entities/select
      (+scope {:lot-note/lots {:id lot-id}} :lot-note authenticated))))
```

`wrap-parse-id-params` middleware already coerces `lot-id` from String to Long.
The entity type is inferred from the `:lot-note/` namespace in the criteria map,
so no explicit `:entity-type` option is needed.

### Related Files
- `src/clj_money/api/lot_notes.clj` — working example
- `src/clj_money/db/datomic/queries.clj` — Datomic relationships config
- `src/clj_money/db/sql/queries.clj` — SQL join config with `[:any ...]`
- `stowaway/src/stowaway/criteria.clj` — `::criteria-value` spec
- `stowaway/src/stowaway/datalog.clj` — `normalize-criterion` for model-refs
