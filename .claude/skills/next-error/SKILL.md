---
name: next-error
description: Fetch the most recent error from honeybadger and resolve it.
user-invocable: true
disable-model-invocation: true
argument-hint: <environment>
---

# Fetch next error

Fetch the most recent error from HoneyBadger and resolve it.

Steps:

1. Call HoneyBadger and get the 5 most recent unresolved errors for the `Money`
   project. Look in the `production` environment unless an argument is supplied,
   in which case use the given environment.
2. Present the error list ot the user for selection. Also include an option to
   abandon the operation here.
3. Create a new git branch for this work.
4. Implement a fix for the error.
5. When the work is complete, push the branch and create a pull request using
   `gh pr create`.
