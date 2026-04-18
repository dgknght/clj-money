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
  (Backlog) and limit 50. The API does not return cards in visual (position)
  order, so select the card with the lowest `pos` value — that is the card
  at the top of the list.
2. Present the card name, description, and label(s) to the user.
3. Create a new feature branch for this work.
4. Call `mcp__trello__move-card` to move the card to the "In Progress" list (id
   `5eb489c9c2840d7d90f0f325`).
5. Confirm the move to the user, then begin implementing the work described in
   the card.
6. Once the work is complete, push the branch and create a pull request using
   `gh pr create`. This repository uses `main` as the default branch.
