---
name: Forgejo CLI
description: General notes on using the fj CLI and Forgejo API against the self-hosted git.dgknght.com instance.
---

# Forgejo CLI

CI and PR tracking for this repo runs on the self-hosted Forgejo instance at
`git.dgknght.com`, via the `fj` CLI and the `CLAUDE_FORGEJO_ACCESS_TOKEN` env
var (a Forgejo access token).

`fj`'s automatic repo/host detection from the git remote is unreliable here:
it resolves the remote's SSH host through `~/.ssh/config`, which aliases
`git.dgknght.com` to an internal hostname that isn't valid for HTTPS API
calls. Always pass `-H git.dgknght.com --repo dgknght/clj-money` explicitly.

Useful commands:
- `fj pr status "dgknght/clj-money#<PR>" -H git.dgknght.com` -- check status
  of a PR's checks.
- `fj pr search -H git.dgknght.com --repo dgknght/clj-money` -- find a PR
  (matches on title, not branch name -- cross-check against
  `git branch --show-current`, and if still ambiguous, cross-check against
  the PR list from the Forgejo API directly).

Forgejo's public API (`/swagger.v1.json`, found at the site root, not under
`/api/v1/`) only exposes run-level Actions status
(`/repos/{owner}/{repo}/actions/runs`, `/actions/tasks`) -- no endpoint for
job-level status or logs. `.claude/scripts/forgejo_ci_failures.py` works
around this via two undocumented web routes (verified working with an
`Authorization: token` header):
- `GET /{owner}/{repo}/actions/runs/{run}/jobs/{index}/attempt/{n}` -- HTML
  page with job status embedded in a `data-initial-post-response` attribute.
- `GET /{owner}/{repo}/actions/runs/{run}/jobs/{index}/attempt/{n}/logs` --
  raw log text.

See the comment at the top of that script for more detail. If either route
errors out (e.g. it changed on a Forgejo upgrade), fall back to opening the
run in a browser, or ask the user to download the log from the Actions run
page and provide the file.

Forgejo Projects (kanban boards, e.g. the "Development" board at
`/dgknght/clj-money/projects/2`) has no REST API or `fj` support at all on
this instance (v15.0.6+gitea-1.22.0) -- confirmed by grepping the full
`swagger.v1.json` for `project`/`board`/`kanban` and finding nothing beyond
the repo's `has_projects` unit-toggle field. Two things are still possible
via undocumented web routes accepting the same `Authorization: token`
header:
- **Reading column membership**: the project board page
  (`GET /{owner}/{repo}/projects/{project}`) is server-rendered HTML with
  each column as a `<div class="project-column" ... data-id="N">` block
  containing a `project-column-title-label` and one `issue-card` per card,
  in actual board (drag) order, each linking to `/{owner}/{repo}/issues/{n}`.
  `.claude/scripts/forgejo_project_column.py` scrapes this to list a named
  column's issues in order.
- **Adding issues to a column in bulk**: there's no way to place an issue
  into a specific column directly, but each column has a "Set default"
  toggle (governs where *newly added* project issues land — it does not
  retroactively move issues already in the project) and the issue-list page
  (`/issues?labels=<id>`) has row checkboxes plus a bulk "Projects" action.
  So: set the target column default, filter issues by label, select-all,
  bulk-add — they land directly in that column. List pages cap at ~20 rows
  and "select all" only grabs the current page, so paginate and repeat.

**Moving/reordering cards IS scriptable, via the same endpoint SortableJS
uses** -- a synthetic browser drag (single press-move-release) does NOT
trigger Forgejo's sortable.js drag lifecycle (no network request fires,
confirmed via `read_network_requests`), but you don't need a drag at all:
each column's `.ui.cards` container has `data-url="/{owner}/{repo}/projects/{project}/{boardID}"`,
and dragging POSTs to `{that data-url}/move` with a JSON body
`{"issues": [{"issueID": <id>, "sorting": <index>}, ...]}` listing, in
target order, every card that should be considered for that column. This
call can be replayed directly -- e.g. via `javascript_tool`'s `fetch()`
inside an authenticated browser tab (reuses the session cookie, no CSRF
token needed) -- to reorder a column or move a card into it, no drag
required. The response is `{"ok":true}` on success.

The request only ever *adds/reorders* the listed issues into the target
board; it does not purge that board down to exactly the given list, so an
issue already sitting in the column but omitted from the payload is left
alone (harmless when reordering a column you're sending in full).

**Critical gotcha**: the card's `data-issue` attribute (and the `issueID`
field the endpoint expects) is the issue's **global database ID**, *not*
the repo-scoped number shown in the URL/title (`#6` etc). Sending the
visible issue number as `issueID` silently reassigns whatever *unrelated*
issue happens to own that ID elsewhere on the instance into your column --
this actually happened during a reorder and dragged 16 wrong issues into
the wrong column. Get the real ID either by reading the live DOM
(`data-issue="N"` on `.issue-card`) or from the REST API's `id` field
(`GET /api/v1/repos/{owner}/{repo}/issues` -- distinct from `number`).
There's a fixed but *repo-specific and not-to-be-assumed* offset between
the two (e.g. `id = number + 25` was observed for clj-money at migration
time, because that many issues/PRs existed instance-wide beforehand) --
always verify per-repo via the API rather than reusing a remembered
offset.

Note: requests made with Python's default `urllib` User-Agent get blocked by
Cloudflare (`403`, `error code: 1010`) in front of `git.dgknght.com`. Send a
spoofed `User-Agent` (e.g. `curl/8.0`) on any request made outside of `fj`
or `curl` itself.
