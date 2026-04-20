---
name: resolve-deps-conflicts
description: Resolve dependency conflicts in project.clj
user-invocable: true
---

# Resolve Dependency Conflicts

Resolve dependency conflicts in project.clj as reported by `lein deps :tree`.

1. Run `lein deps :tree`.
2. Look at the STDERR output. If there is any, it will be a list of conflicting
   dependency versions and suggestion exclusions to resolve them. If there are
   no conflicts, then nothing needs to be done.
3. Create a feature branch.
4. Pick one of the suggestions and apply it to project.clj.
5. Repeast step 2 until no conflicts are reported.
6. Push the changes to github and create a PR.
