---
name: resolve-deps-conflicts
description: Resolve dependency conflicts in project.clj
user-invocable: true
---

# Resolve Dependency Conflicts

Resolve dependency conflicts in project.clj as reported by `lein deps :tree`.

1. Run `lein deps :tree`.
2. Look at the STDERR output for any conflicting dependency versions and
   suggested exclusions to resolve them. If there are no conflicts, then
   nothing needs to be done. If there is output not related to conflicts,
   report that to the user.
3. Create a feature branch.
4. Pick one of the suggestions and apply it to project.clj.
5. Repeast step 2 until no conflicts are reported.
6. Push the changes to github and create a PR.
