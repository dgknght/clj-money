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
  (Backlog) and limit 50.
2. Select the card with the lowest `pos` value by calling
   `cat <tool-result-file> | python3 .claude/scripts/parse_trello_cards.py`.
3. Present the card name, description, and label(s) to the user and offer these
   options:
   - Start work on this card.
   - Choose another card near the top of the stack.
   - Exit.
4. Create a new feature branch for the chosen card.
5. Call `mcp__trello__move-card` to move the card to the "In Progress" list (id
   `5eb489c9c2840d7d90f0f325`).
6. Do the work described by the card.
7. Once the work is complete, push the branch and create a pull request using
   `gh pr create`. This repository uses `main` as the default branch.
