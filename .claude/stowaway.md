# stowaway

Pluggable CRUD abstraction that decouples application code from the storage
backend. Used in clj-money as the primary database interface.

## Core Protocol (`stowaway.core/Storage`)

| Method | Signature | Description |
|--------|-----------|-------------|
| `create` | `[model]` | Insert a record; returns the created model with id |
| `select` | `[criteria]` or `[criteria opts]` | Query records |
| `update` | `[model]` or `[criteria changes]` or `[criteria changes opts]` | Update records |
| `delete` | `[model]` | Delete a single record |
| `delete-by` | `[criteria]` | Delete records matching criteria |
| `with-transaction` | `[f]` | Execute `f` within a transaction |

Use `register-strategy` to add a storage implementation and `reify-storage`
to construct an instance.

## Model Tagging

```clojure
(storage/tag model :account)   ; attach type metadata to a model map
(storage/tag model)            ; retrieve the type tag
```

Type tags are used by authorization dispatch (`allowed?`, `scope`) to
determine which multimethod implementation to invoke.

## Implicit Storage (`stowaway.implicit`)

Provides a dynamic var and convenience macros so call sites don't need to
thread a storage value explicitly.

```clojure
(with-storage [config]
  (stow/select {:account/entity-id 42}))

(with-transacted-storage [config]
  (stow/create {:account/name "Checking" ...})
  (stow/create {:account/name "Savings" ...}))
```

In clj-money, these are re-exported from `clj-money.db`. Use the aliases
`stow/select`, `stow/create`, `stow/update`, `stow/delete`, etc. in
application code.

## Criteria DSL (`stowaway.criteria`)

Criteria are passed to `select`, `update`, and `delete-by`.

### Map Equality

```clojure
{:account/entity-id 42}                   ; simple equality
{:transaction/account {:id 7}}            ; match by foreign key ref
```

### Operator Vectors

```clojure
{:price/value [:>= 100]}
{:account/name [:like "sa%"]}
{:transaction/date [:between (t/local-date 2024 1 1)
                              (t/local-date 2024 12 31)]}
```

### Compound Criteria

```clojure
[:and {:account/type :asset} {:account/active true}]
[:or  {:account/type :asset} {:account/type :liability}]
```

### Supported Operators

| Operator | Meaning |
|----------|---------|
| `:=` | equal (default for scalar values) |
| `:!=` | not equal |
| `:>` `:>=` `:<` `:<=` | numeric/date comparison |
| `:like` | SQL LIKE pattern |
| `:in` | value in collection |
| `:any` | any element matches |
| `:&&` | array overlap |
| `:between` | exclusive on both ends |
| `:<between` | exclusive lower bound |
| `:between>` | exclusive upper bound |
| `:<between>` | inclusive on both ends |
| `:contained-by` | contained by array/range |
| `:including` | array includes value |
| `:including-match` | array includes matching element |

## Criteria Helpers (`stowaway.criteria`)

| Function | Description |
|----------|-------------|
| `namespaces` | Return all namespaces present in criteria map |
| `extract-ns` | Extract criteria for a specific namespace |
| `single-ns` | Assert criteria contains exactly one namespace |
| `apply-to` | Apply a function to matching criteria values |
| `update-in` | Criteria-aware `update-in` |
| `simplify-and` | Collapse single-item `:and` to bare criteria |
