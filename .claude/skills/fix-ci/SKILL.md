---
name: fix-ci
description: Read Forgejo Actions failures for the current PR and fix them.
user-invocable: true
disable-model-invocation: true
---

# Fix CI failures

Read the Forgejo Actions failures for the current PR and fix them.

See the `forgejo-cli` skill for general notes on the `fj` CLI and the
Forgejo API quirks this depends on.

Steps:

1. Run `fj pr status "dgknght/clj-money#<PR>" -H git.dgknght.com` to confirm
   which check is failing for the current branch's PR. If you don't know the
   PR number, find it with
   `fj pr search -H git.dgknght.com --repo dgknght/clj-money`.
2. Run `python3 .claude/scripts/forgejo_ci_failures.py` to get the failure
   log for the most recent run of the current branch's PR (pass `--branch`
   or `--pr` to target a different one). This prints the log for every
   failed job, with noisy dependency-download lines filtered out.
3. Analyze the failures and identify the root cause(s).
4. Fix the issues in the code.
5. Run the relevant tests locally to confirm the fix (use `lein ptest <namespace>`
   for the smallest subset that covers the failure).
6. Run `clj-kondo --lint src` and resolve any warnings before committing.
7. Commit the fix with a descriptive message.
8. Push the branch with the new commits.
