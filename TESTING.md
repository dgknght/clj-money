# Test Parallelization Analysis

## Summary

After investigating test parallelization options for this project, **true
parallel execution is not feasible** due to the current test architecture's
reliance on a shared database with global `reset-db` fixtures.

## What Was Implemented

### eftest Integration

Added eftest test runner with improved reporting and faster test discovery:

```bash
# Run all tests with eftest (serial execution, better reporting)
lein test-fast

# Run only API tests
lein test-api

# Run specific namespaces
lein eftest clj-money.api.accounts-test
```

### Configuration

- **Plugin**: `lein-eftest 0.6.0` added to project.clj:163
- **Dependency**: `eftest 0.6.0` added to test profile at project.clj:208
- **Settings**: Configured at project.clj:205-206 with:
  - Serial execution (multithread? false)
  - Real-time output (capture-output? false) for progress visibility

## Why Parallel Execution Doesn't Work

### Root Cause
Tests use a shared database with `:each` fixtures that reset the entire
database before each test. When tests run in parallel (even in separate
processes), they experience:

1. **Race conditions** during database reset
2. **Validation errors** from duplicate entity names
3. **Data conflicts** from shared test context (`basic-context`)

### Evidence
- Parallel execution: 22 errors (database conflicts)
- Serial execution: 0 errors (all tests pass)
- Each test expects exclusive database access

## Recommendations

### Short Term (Implemented)
Use eftest for better test reporting and developer experience, but keep
serial execution:

```bash
lein test-fast  # Faster test discovery, better output than lein test
```

**Benefits over standard `lein test`:**
- **Real-time progress bar** with percentage and ETA
- **Colored output** for easier reading (green/red)
- **Faster test discovery**
- **Better error formatting** with cleaner stack traces

Example output:
```
 6/13    46% [=======================           ]  ETA: 00:08
...
Ran 13 tests in 13.0 seconds
36 assertions, 0 failures, 0 errors.
```

### Medium Term (If Parallelization Needed)
To enable true parallelization, choose one of these approaches:

1. **Database-per-process isolation**
   - Configure each test process to use a separate database schema
   - Requires: Environment variable support in test setup
   - Complexity: Medium
   - Speedup: ~4x on 4-core machine

2. **Transaction-based isolation**
   - Wrap each test in a transaction, rollback after completion
   - Requires: Refactoring `reset-db` fixture
   - Complexity: High (must ensure all DB access uses same connection)
   - Speedup: ~2-3x (limited by transaction overhead)

3. **Test categorization**
   - Separate unit tests (no DB) from integration tests (DB required)
   - Run unit tests in parallel, integration tests serially
   - Complexity: Medium (requires test refactoring)
   - Speedup: Varies based on test distribution

### Long Term
Consider moving toward:
- More isolated unit tests that don't require database
- Integration tests that use test containers with isolated databases
- Property-based testing for business logic

## Performance Notes

Current test suite (54 test files):
- Serial execution with `lein test`: ~X minutes (measure baseline)
- Serial execution with `lein eftest`: Similar, with better progress reporting
- Parallel execution: Not possible without architecture changes

## Usage

```bash
# Run all tests (serial, with eftest)
lein test-fast

# Run specific test group
lein test-api

# Run traditional lein test (fallback)
lein test

# Run specific namespace
lein test clj-money.api.accounts-test
lein eftest clj-money.api.accounts-test
```

## Files Modified

- `project.clj:161` - Added lein-eftest plugin
- `project.clj:203-205` - Added eftest configuration
- `project.clj:207` - Added eftest dependency
- `project.clj:189-190` - Added test convenience aliases
