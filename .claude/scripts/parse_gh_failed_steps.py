#!/usr/bin/env python3
"""Parse GitHub Actions jobs JSON and print failed step names and their job IDs.

Usage:
  gh api /repos/OWNER/REPO/actions/runs/RUN_ID/jobs | python3 parse_gh_failed_steps.py
"""
import json
import sys

data = json.load(sys.stdin)
for job in data["jobs"]:
    for step in job["steps"]:
        if step.get("conclusion") == "failure":
            print(f"job_id={job['id']} step={step['name']!r}")
