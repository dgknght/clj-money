---
name: next
description: Fetch the next backlog issue from the Forgejo "Development" project board's Backlog column and begin working on it.
user-invocable: true
disable-model-invocation: true
---

# Fetch next work item

Fetch the next item from the Backlog column of the "Development" Forgejo
project board (`dgknght/clj-money`, project #2) and begin working on it. See
the `forgejo-cli` skill for general `fj`/API notes.

Steps:

1. Run `python3 ~/.claude/scripts/forgejo_project_column.py "Backlog"` to get
   the issue numbers currently in the Backlog column, top to bottom, in
   actual board order. There's no REST API or `fj` support for Forgejo
   Projects, so this scrapes the project board's HTML directly — see the
   comment at the top of that script for the fragility caveat and a browser
   fallback. Board order reflects manual drag-to-reorder on the board, so
   the first number in the output is the top of the stack.
2. Fetch the full body/labels for that issue: `fj issue view
   "dgknght/clj-money#<ISSUE>" body -H git.dgknght.com` (`view`, like `edit`,
   has no `--repo` flag — the repo is part of the issue reference).
3. Present the issue title, body, and label(s) to the user and offer these
   options:
   - Start work on this issue.
   - Choose another issue near the top of the stack.
   - Exit.
4. Create a new feature branch for the chosen issue (see the `new-branch`
   skill).
5. Swap its state label to reflect the move: `fj issue edit
   "dgknght/clj-money#<ISSUE>" labels -H git.dgknght.com -a status/in-progress
   -r status/backlog`. This does not move the card on the project board —
   only dragging does that, and drag-and-drop isn't scriptable headlessly
   (see the `forgejo-cli` skill for why). If you want the board's In Progress
   column to reflect the claim, drag the card there yourself; the label swap
   is the only automated part of this step.
6. Do the work described by the issue.
7. Once the work is complete, push the branch and create a pull request:
   `fj pr create -H git.dgknght.com --repo dgknght/clj-money --autofill
   --base main`. Include "fixes #<ISSUE>" in the commit message.
