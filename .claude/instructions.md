# Clojure Code Style Guidelines for clj-money

## General Clojure Conventions

### Naming
- Use kebab-case for functions, variables, and namespaces
- Prefix predicates with `?` (e.g., `valid?`, `empty?`)
- Use `->` prefix for conversion/transformation functions (e.g., `->attribute-key`)
- Use descriptive names that convey intent
- Do not use more words than necessary to convey intent within the current context

### Code Organization
- Keep namespaces focused and cohesive
- Use `require` with aliases for external dependencies
- Import Java classes explicitly in `:import` declarations
- Prefer pure functions where possible

### Error Handling
- Use meaningful error messages
- Prefer explicit error handling over silent failures

## Project-Specific Conventions

### OpenTelemetry/Observability
- Use the `clj-money.otel/with-tracing` macro for custom spans
- Use `clj-money.otel/set-attribute` for adding span attributes
- Attribute keys should follow snake_case convention (handled by helper)
- Keep span names descriptive and operation-focused

### Testing
- Use `clojure.test` framework
- Test files mirror source structure with `_test` suffix
- Write focused unit tests with clear assertions

### Dependencies
- Keep dependency exclusions explicit and documented when needed
- Prefer stable, well-maintained libraries

## Development Workflow

### REPL Development
- Use `lein clean` if encountering stale compilation issues
- Reload namespaces with `:reload` flag when making changes
- Remember that macro changes require reloading both the defining namespace and consuming namespaces

### Environment Configuration
- Use environment variables for runtime configuration (OpenTelemetry, database, etc.)
- Keep sensitive data out of source control
- Document required environment variables
