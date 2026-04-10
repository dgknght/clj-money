---
name: next
description: Fetch the next item from the clj-money Trello Backlog and begin working on it.
user-invocable: true
disable-model-invocation: true
---

Fetch the next item from the clj-money Trello Backlog and begin working on it.

Steps:
1. Call `mcp__trello__get-tickets-by-list` with listId `5eb489c19f703934207bcdd9` (Backlog) and limit 1.
2. Present the card name, description, and label(s) to the user.
3. Create a new feature branch for this work. If we are not on the `main` branch, confirm with the user.
4. Call `mcp__trello__move-card` to move the card to the "In Progress" list (id `5eb489c9c2840d7d90f0f325`).
5. Confirm the move to the user, then begin implementing the work described in the card.
