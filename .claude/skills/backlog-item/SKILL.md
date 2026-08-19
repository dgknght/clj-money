---
name: backlog-item
description: File a Forgejo issue on dgknght/clj-money, labeled from:backlog, for follow-up work that shouldn't be done right now.
---

# Backlog Item

Use this when work is identified during a task but is out of scope for right
now (e.g. a risk to track, a future refactor, a dependency to replace) and
should be captured for later instead of acted on immediately.

Task tracking for this repo lives in Forgejo issues on `dgknght/clj-money`
(migrated from Trello — see the `forgejo-cli` skill for general `fj`/API
notes). A card's Trello-list-equivalent "state" is tracked with a
`from:*` label (`from:ice-box`, `from:backlog`, `from:in-progress`,
`from:pending-delivery`) rather than a kanban column, since this Forgejo
instance doesn't support scripting its Projects (kanban) feature.

Steps:

1. Create the issue:
   `fj issue create -H git.dgknght.com --repo dgknght/clj-money "<TITLE>" --body "<BODY>"`
   - `<TITLE>`: a short, specific title (what needs to happen, not just the
     symptom).
   - `<BODY>`: enough context for a future session with no memory of this
     conversation to act on it — what was found, why it matters, why it
     wasn't fixed now, and any concrete leads (file paths, alternatives
     considered, links).
2. Label it: `fj issue edit "dgknght/clj-money#<ISSUE>" labels -H git.dgknght.com -a from:backlog`.
   (`fj issue edit` has no `--repo` flag — unlike `create`/`search`/`status`,
   the repo is passed as part of the issue reference itself.)
   If one of the existing type labels (`defect`, `feature`, `housekeeping`,
   `refactor`, `budget`, `investment`, `experimental`) obviously applies,
   add it too (`-a <label>`, repeatable).
3. Report the created issue's URL back to the user.
