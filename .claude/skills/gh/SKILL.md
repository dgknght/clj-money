---
name: Github CLI
description: Be sure the we're using the right context before we run any `gh` commands.
---

# Github CLI

Ensure that we have the right context before running the `gh` command.

1. Ensure the the `gh` utility is available with `which gh`. If not, advise the
   user on how to install it.
2. Run `gh auth status`. We should be using the account $GH_ACCOUNT_KSS. If we
   are not, call `gh auth switch` until we are. If the cycle repeats and we
   haven't found the acount we want, advise the user that we need to authenticate
   with that account.
