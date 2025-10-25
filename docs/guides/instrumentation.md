# Instrumentation Guidelines

## When to Add Tracing

Add tracing for operations that:
1. **Take significant time** (> 10ms typically)
2. **Make external calls** (HTTP, database, file I/O)
3. **Are business-critical** (core workflows)
4. **Can fail** (need to track errors)
5. **Have multiple steps** (need to see breakdown)

## When NOT to Add Tracing

Avoid tracing for:
1. **Pure functions** with minimal computation
2. **Getters/setters** (low value, high overhead)
3. **Tight loops** (creates too many spans)
4. **Very frequent operations** (unless using sampling)

## Span Naming Conventions

Use consistent naming patterns:

```
{domain}/{operation}
```

Examples:
- `yahoo/fetch-price`
- `import/transactions`
- `export/transactions`
- `email/send`
- `reconciliation/reconcile-account`
- `report/income-statement`
- `cache/get`

## Attribute Guidelines

Add attributes for:
- **Identifiers**: user ID, account ID, transaction ID
- **Metadata**: format, type, status
- **Metrics**: counts, durations, sizes
- **Context**: dates, amounts, categories

Avoid:
- **High cardinality**: Don't use unique IDs for metrics labels
- **PII**: Don't include passwords, SSN, credit cards
- **Large data**: Don't include full documents or large payloads
