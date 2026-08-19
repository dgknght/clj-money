#!/usr/bin/env python3
"""Print the issue numbers in one column of the "Development" Forgejo
project board (dgknght/clj-money, project #2), in board order.

Forgejo's public API has no endpoint for Projects (kanban boards) -- see
the forgejo-cli skill for details. This scrapes the project board's
server-rendered HTML instead: each column is a
  <div class="project-column" ... data-id="N"> ... <span
  class="project-column-title-label">NAME</span> ...
containing one issue-card per card in board (drag) order, each with an
  <a class="issue-card-title" ... href="/dgknght/clj-money/issues/N">
This is undocumented markup and may break on a Forgejo upgrade -- if this
script starts erroring or returning nothing, open
https://git.dgknght.com/dgknght/clj-money/projects/2 in a browser instead.

Usage:
  python3 .claude/scripts/forgejo_project_column.py "Backlog"

Requires the CLAUDE_FORGEJO_ACCESS_TOKEN environment variable.
"""
import os
import re
import sys
import urllib.error
import urllib.request

HOST = "git.dgknght.com"
OWNER = "dgknght"
REPO = "clj-money"
PROJECT = 2


def fetch_board():
    token = os.environ.get("CLAUDE_FORGEJO_ACCESS_TOKEN")
    if not token:
        sys.exit("CLAUDE_FORGEJO_ACCESS_TOKEN is not set")
    url = f"https://{HOST}/{OWNER}/{REPO}/projects/{PROJECT}"
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
            return resp.read().decode("utf-8", errors="replace")
    except urllib.error.HTTPError as e:
        sys.exit(f"{e.code} {e.reason} fetching {url}")


def column_issue_numbers(html, column_name):
    columns = list(re.finditer(r'<div class="project-column"[^>]*data-id="(\d+)"', html))
    if not columns:
        sys.exit(
            "No project columns found in the board page -- markup may have "
            f"changed. Check https://{HOST}/{OWNER}/{REPO}/projects/{PROJECT} "
            "in a browser."
        )
    for i, m in enumerate(columns):
        start = m.end()
        end = columns[i + 1].start() if i + 1 < len(columns) else len(html)
        block = html[start:end]
        title = re.search(r'project-column-title-label">([^<]+)<', block)
        if title and title.group(1).strip() == column_name:
            return re.findall(rf'href="/{OWNER}/{REPO}/issues/(\d+)"', block)
    available = []
    for i, m in enumerate(columns):
        start = m.end()
        end = columns[i + 1].start() if i + 1 < len(columns) else len(html)
        title = re.search(r'project-column-title-label">([^<]+)<', html[start:end])
        if title:
            available.append(title.group(1).strip())
    sys.exit(f"Column {column_name!r} not found. Available columns: {available}")


def main():
    if len(sys.argv) != 2:
        sys.exit("Usage: forgejo_project_column.py <column-name>")
    numbers = column_issue_numbers(fetch_board(), sys.argv[1])
    for n in numbers:
        print(n)


if __name__ == "__main__":
    main()
