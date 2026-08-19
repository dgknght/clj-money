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

Note: requests made with Python's default `urllib` User-Agent get blocked by
Cloudflare (`403`, `error code: 1010`) in front of `git.dgknght.com`. Send a
spoofed `User-Agent` (e.g. `curl/8.0`) on any request made outside of `fj`
or `curl` itself.
