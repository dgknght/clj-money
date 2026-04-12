---
name: next
description: Fetch the next item from the clj-money Trello Backlog and begin working on it.
user-invocable: true
disable-model-invocation: true
---

# Fetch next work item

Fetch the next item from the clj-money Trello Backlog and begin working on it.

Steps:

1. Call `mcp__trello__get-tickets-by-list` with listId `5eb489c19f703934207bcdd9`
  (Backlog) and limit 1.
2. Present the card name, description, and label(s) to the user.
3. Run `git branch --show-current` to determine the actual current branch (do not
   rely on the gitStatus from the start of the session, which may be stale).
   If we are not on `main`, confirm with the user before proceeding.
4. Create a new feature branch for this work. Try to limit the branch name to
   three or four words.
5. Call `mcp__trello__move-card` to move the card to the "In Progress" list (id
   `5eb489c9c2840d7d90f0f325`).
6. Confirm the move to the user, then begin implementing the work described in
   the card.
7. Once the work is complete, push the branch and create a pull request using
   `gh pr create`.
