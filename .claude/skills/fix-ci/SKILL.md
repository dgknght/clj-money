---
name: fix-ci
description: Read GitHub Actions failures for the current PR and fix them.
user-invocable: true
disable-model-invocation: true
---

# Fix CI failures

Read the GitHub Actions failures for the current PR and fix them.

Steps:

1. Run `gh pr view --json number,headRefName` to confirm the current PR and branch.
2. Run `gh run list --branch <branch> --limit 5` to find recent workflow runs.
3. Run `gh run view <run-id> --log-failed` to get the failure output for the most
   recent failed run.
4. Analyze the failures and identify the root cause(s).
5. Fix the issues in the code.
6. Run the relevant tests locally to confirm the fix (use `lein ptest <namespace>`
   for the smallest subset that covers the failure).
7. Run `clj-kondo --lint src` and resolve any warnings before committing.
8. Commit the fix with a descriptive message.
9. Push the branch with the new commits.
