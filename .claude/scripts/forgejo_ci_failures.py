#!/usr/bin/env python3
"""Fetch Forgejo Actions failure logs for a PR's most recent CI run.

Usage:
  python3 .claude/scripts/forgejo_ci_failures.py [--branch BRANCH] [--pr N] [--remote origin]

Requires the CLAUDE_FORGEJO_ACCESS_TOKEN environment variable (a Forgejo
access token). Defaults to the current git branch and the "origin" remote.

Forgejo's public API (see /swagger.v1.json on the instance) does not expose
job-level status or log content for Actions runs -- only run-level status
via /repos/{owner}/{repo}/actions/tasks. The job list (with per-job status
and index) and the raw log text are fetched from undocumented web routes
that happen to accept the same "Authorization: token" header as the API:

  GET /{owner}/{repo}/actions/runs/{run}/jobs/{index}/attempt/{attempt}
      -> HTML page; a data-initial-post-response attribute embeds JSON
         with the full, ordered list of jobs for the run (id, name,
         status, duration).
  GET /{owner}/{repo}/actions/runs/{run}/jobs/{index}/attempt/{attempt}/logs
      -> plain text log for that job.

These routes could change on a Forgejo upgrade since they aren't part of
the documented API. If this script starts failing, fall back to opening
the run in a browser (the URL printed on failure) and downloading the log
manually.
"""
import argparse
import html
import json
import os
import re
import subprocess
import sys
import urllib.error
import urllib.request

NOISE_RE = re.compile(r"Retrieving .+\.(pom|jar) from")


def git_remote_url(remote):
    return subprocess.check_output(
        ["git", "remote", "get-url", remote], text=True
    ).strip()


def parse_owner_repo_host(url):
    m = re.search(r"(?:@|://)([^:/@]+)(?::\d+)?[:/]([^/]+)/([^/]+?)(?:\.git)?$", url)
    if not m:
        sys.exit(f"Could not parse owner/repo/host from remote url: {url}")
    host, owner, repo = m.group(1), m.group(2), m.group(3)
    return host, owner, repo


def current_branch():
    return subprocess.check_output(
        ["git", "rev-parse", "--abbrev-ref", "HEAD"], text=True
    ).strip()


def request(url, token, as_json):
    # A default Python User-Agent gets blocked (Cloudflare error 1010) in
    # front of this instance; a plain, non-Python-looking UA passes.
    req = urllib.request.Request(
        url,
        headers={
            "Authorization": f"token {token}",
            "User-Agent": "curl/8.0",
        },
    )
    try:
        with urllib.request.urlopen(req, timeout=60) as resp:
            body = resp.read().decode("utf-8", errors="replace")
    except urllib.error.HTTPError as e:
        sys.exit(f"{e.code} {e.reason} fetching {url}")
    return json.loads(body) if as_json else body


def api(host, token, path):
    return request(f"https://{host}/api/v1{path}", token, as_json=True)


def raw(host, token, path):
    return request(f"https://{host}{path}", token, as_json=False)


def find_pr_number(host, token, owner, repo, branch):
    prs = api(host, token, f"/repos/{owner}/{repo}/pulls?state=open&limit=50")
    matches = [pr for pr in prs if pr["head"]["ref"] == branch]
    if not matches:
        sys.exit(f"No open PR found for branch '{branch}' in {owner}/{repo}")
    return matches[0]["number"]


def latest_run_number(host, token, owner, repo, pr_number):
    tasks = api(host, token, f"/repos/{owner}/{repo}/actions/tasks?limit=50")[
        "workflow_runs"
    ]
    pr_tasks = [t for t in tasks if t.get("head_branch") == f"#{pr_number}"]
    if not pr_tasks:
        sys.exit(f"No CI runs found for PR #{pr_number}")
    return max(t["run_number"] for t in pr_tasks)


def fetch_jobs(host, token, owner, repo, run_number):
    page = raw(
        host, token, f"/{owner}/{repo}/actions/runs/{run_number}/jobs/0/attempt/1"
    )
    m = re.search(r'data-initial-post-response="([^"]*)"', page)
    if not m:
        sys.exit(
            "Could not find job data in the run page -- Forgejo's web UI markup "
            "may have changed. Fall back to viewing the run in a browser: "
            f"https://{host}/{owner}/{repo}/actions/runs/{run_number}"
        )
    payload = json.loads(html.unescape(m.group(1)))
    return payload["state"]["run"]["jobs"]


def print_failed_logs(host, token, owner, repo, run_number, jobs):
    failed = [j for j in jobs if j["status"] == "failure"]
    if not failed:
        print(f"No failed jobs in run #{run_number}.")
        return
    for job in failed:
        idx = jobs.index(job)
        print(f"=== Job: {job['name']} (failed, {job['duration']}) ===")
        log = raw(
            host,
            token,
            f"/{owner}/{repo}/actions/runs/{run_number}/jobs/{idx}/attempt/1/logs",
        )
        for line in log.splitlines():
            if not NOISE_RE.search(line):
                print(line)
        print()


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--branch", default=None)
    ap.add_argument("--pr", type=int, default=None)
    ap.add_argument("--remote", default="origin")
    args = ap.parse_args()

    token = os.environ.get("CLAUDE_FORGEJO_ACCESS_TOKEN")
    if not token:
        sys.exit("CLAUDE_FORGEJO_ACCESS_TOKEN is not set")

    host, owner, repo = parse_owner_repo_host(git_remote_url(args.remote))
    branch = args.branch or current_branch()

    pr_number = args.pr or find_pr_number(host, token, owner, repo, branch)
    run_number = latest_run_number(host, token, owner, repo, pr_number)
    jobs = fetch_jobs(host, token, owner, repo, run_number)
    print_failed_logs(host, token, owner, repo, run_number, jobs)


if __name__ == "__main__":
    main()
