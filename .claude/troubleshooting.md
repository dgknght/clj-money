# Troubleshooting

Known problems and their fixes, organized by symptom. The user-facing
version lives in the wiki:
https://git.dgknght.com/dgknght/clj-money/wiki/Troubleshooting

## Tests

### `Assert failed: (instance? QualifiedID qid)` in SQL tests

The `-sql` variants of tests (e.g. in `entities-test`) error with

```
java.lang.AssertionError: Assert failed: (instance? QualifiedID qid)
  at clj_money.db.sql$find_STAR_ (sql.clj:452)
```

This is usually stale compiled output in `target/`, not a code problem.
Run `lein clean` and then re-run the tests before investigating further.

### `Validation failed ... #:entity{:user {:id ["Id is invalid"]}}` in SQL tests

The `-sql` variants of tests error while `with-context` creates the entity.
Like the `QualifiedID` assertion above, this has been caused by stale
compiled output; `lein clean` fixed it.

### `PSQLException: column "..." of relation "..." does not exist` in SQL tests

The `-sql` variants of tests error because the test database is missing a
recently added migration. Apply it with `lein with-profile test migrate`
(the test config ignores the development database environment variables).

## Client

### Blank page with `Bad dependency path or symbol: chartjs` (or `opentelemetry`)

The dev build loads, but the browser console reports a bad dependency path
for one of the foreign libs declared in `src/deps.cljs`, and
`resources/public/cljs-out/dev/cljs_deps.js` has no `goog.addDependency`
entry for `../js/chart-bundle.js` or `../js/otel-bundle.js`.

The ClojureScript compiler memoizes what it reads from `deps.cljs`
(`cljs.closure/get-upstream-deps`) for the life of the JVM, so a figwheel
started in a REPL that predates a new foreign lib never picks it up, even
after stopping and starting figwheel. Run `npm install` if the bundle in
`resources/js` is missing, then quit that REPL and start a new one.

### `ReferenceError: chartjs is not defined` (or `opentelemetry`)

The foreign lib loads, but the namespace that requires it was compiled by
a JVM that didn't know about it yet, so its output in
`resources/public/cljs-out/dev` refers to a bare `chartjs` global instead
of the bundle's `cljMoneyChart` (see `:global-exports` in `src/deps.cljs`).
Figwheel reuses that output until the source changes. Stop figwheel, run
`lein clean` (which clears `resources/public/cljs-out`), and start it again.

### Blank page in production with `GET /cljs-out/clj-money.js 404`

The uberjar was built without the compiled client app. `lein uberjar` runs
`lein clean` first, and `:clean-targets` includes `resources/public/cljs-out`
(see above), so it deletes what `lein fig:prod` just compiled. The `:uberjar`
profile in `project.clj` replaces `:clean-targets` with `[:target-path]` to
prevent this; check that it's still there. `unzip -l target/clj-money.jar |
grep cljs-out` should list `public/cljs-out/clj-money.js`.

### A link reloads the whole page, losing in-memory state

`accountant` intercepts a click by finding the nearest element with an
`href` attribute. An icon's `<use href="/images/bootstrap-icons.svg#...">`
is such an element, so a click on the glyph of an icon inside a link wasn't
intercepted and the browser loaded the page from the server, losing
anything left in `app-state` for the next page (e.g., the
`:pending-reconciliation` hand-off from the Statements page). `site.scss` gives `a .bi` `pointer-events:
none` so the click lands on the link. Rebuild the CSS locally with `sass
src/scss/site.scss resources/public/css/site.css`.

### New client code isn't running, or the nav throws `No protocol method IDeref.-deref defined for type undefined`

The browser is running stale compiled files: the service worker serves
`/cljs-out/...` from its cache first, and the server sends them without a
`Cache-Control` header, so the HTTP cache may keep them too. In DevTools,
check "Bypass for network" (Application → Service workers) and "Disable
cache" (Network), then reload.

### A client route with a `:param` shows Page Not Found

secretary 1.2.3 extracts the params of a string route like
`"/transactions/:id"` with `(merge-with vector {} (partition 2 ...))`, which
current ClojureScript rejects (`No protocol method IMapEntry.-key defined
for type cljs.core/LazySeq`). Dispatch throws, and the page is never set.
Use a regex route instead, e.g.
`(secretary/defroute #"/transactions/([^/]+)" [id query] ...)`. The captured
groups bind in order, followed by `{:query-params {...}}` when the URL has a
query string. Bind plain symbols only, since `defroute` also uses the
bindings as `{:keys [...]}`. `views/transactions/page_test.cljs` dispatches
its paths to catch this.

A URL with a query string (e.g. `/search?description=...`) also showed Page
Not Found, because `core/dispatch-or-not-found` looked the route up with
the query attached. It now looks up the path alone.

### Server tests fail with `No implementation of method: ... of protocol: #'clj-money.accounts/ValuationData`

Stale AOT-compiled classes in `target/classes/clj_money` define the protocol
separately from the one loaded from source, so reified implementations no
longer satisfy it. Delete `target/classes/clj_money` and rerun.

### Tests fail to start with ``Insufficient `com.taoensso/encore` version``

After a dependency update (e.g. carmine 3.6.0, which needs encore 3.172.1),
`lein test` can fail with ``Insufficient `com.taoensso/encore` version
(v3.158.0 < v3.172.1)`` even though `lein deps :tree` shows the right
version. The old encore is in stale AOT-compiled classes under
`target/classes/taoensso`, which take precedence over the jar. Delete
`target/classes` and rerun; `project.clj` doesn't need to change.
