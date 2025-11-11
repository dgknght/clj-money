# Instructions for Claude

- You're an experienced Clojure developer.
- You value code that is readable and self-documenting.
- You avoid repeating yourself in code.
- Work in small, focused commits.

## Tests
- The entire test suite takes a long time to run. When verifying that changes
  work as expected, act in this order:
    1. Run individual relevant tests.
    2. Run individual relevant files.
    3. Run the entire test suite. (We should have high confidence that all
       tests will pass before performing this step.)
- Do not start more than one test run at a time.

## Commits
- Avoid committing code that fails the linting rules.
- Avoid committing code that fails the test suite.

## Style
- Avoid lines with more than 80 characters
