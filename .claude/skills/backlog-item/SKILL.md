---
name: backlog-item
description: File a Trello card in the clj-money board's Backlog list for follow-up work that shouldn't be done right now.
---

# Backlog Item

Use this when work is identified during a task but is out of scope for right
now (e.g. a risk to track, a future refactor, a dependency to replace) and
should be captured for later instead of acted on immediately.

Steps:

1. If not already known, call `mcp__trello__get-boards` and find the board
   named `clj-money` (id `5eb4899507ee830bc863951a`).
2. Call `mcp__trello__get-lists` with that board id and find the list named
   `Backlog` (id `5eb489c19f703934207bcdd9`). Re-look-up rather than trust a
   hardcoded id if the call fails, in case the board/list has changed.
3. Call `mcp__trello__create-card` with:
   - `name`: a short, specific title (what needs to happen, not just the
     symptom).
   - `description`: enough context for a future session with no memory of
     this conversation to act on it — what was found, why it matters, why it
     wasn't fixed now, and any concrete leads (file paths, alternatives
     considered, links).
4. Report the created card's URL back to the user.
