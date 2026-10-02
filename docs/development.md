# Development Mode

## Tools

1. Install [mise](https://mise.jdx.dev/getting-started.html) and run `mise install` in the project root. This installs Java, Node, lein, and clj-kondo automatically.
2. Install [Podman](https://podman.io/docs/installation) and [podman-compose](https://github.com/containers/podman-compose).

## Setup

1. Create `env/dev/config.edn` and `env/docker/config.edn` by copying `env/test/config.edn` and adjusting:
   - The database details (change `:dbname` to the dev database name)
   - The image storage details (change `:dbname` to the dev database name)
   - OAuth keys: `:google-client-id` and `:google-client-secret`
   - Add `:dev? true`, remove `:test? true`
   - Change `:site-protocol` to `"http"`

   To suppress outgoing email during local development, omit `:mailer-enabled?` or set it to `false`.

2. Add a `env/docker/transactor.properties` file

  ```bash
  host=0.0.0.0
  alt-host=datomic-transactor
  port=4334

  ping-host=0.0.0.0
  ping-port=9999

  protocol=sql
  sql-user=app_user
  sql-password=<app user password>
  sql-url=jdbc:postgresql://sql:5432/datomic
  sql-driver-class=org.postgresql.Driver

  memory-index-threshold=64m
  memory-index-max=1024m
  object-cache-max=512m

  memcached=memcached:11211
  ```

3. Run the full setup task:

   ```bash
   mise run setup
   ```

   You will be prompted for your sudo password to install the PostgreSQL client. After that, the following run in parallel:
   - Starts Podman containers (datomic-peer profile: PostgreSQL, Redis, Memcached, Datomic transactor)
   - Installs Clojure and JS dependencies
   - Installs the sass CLI and does an initial compile
   - Downloads the OpenTelemetry Java agent
   - Initializes clj-kondo configs for all dependencies
   - Configures git to use the repo's pre-push hook (runs the linter before each push)

   Datomic schema initialization runs automatically inside the Docker stack.

### SQL storage strategy

If you need to work with the SQL storage strategy, also run:

```bash
mise run sql-setup
```

This creates and migrates both the development and test SQL databases.

## Running the app

Start the backend REPL:

```bash
lein repl
```

Then manage the system (storage, image storage, progress tracking, external
service configuration and the web server) from the REPL with [integrant.repl](https://github.com/weavejester/integrant-repl):

```clojure
(go)    ; start the system
(reset) ; reload changed namespaces and restart the system
(halt)  ; stop the system
```

The helpers in `clj-money.repl` (`create-user`, `propagate-all`, etc.) use
the running system's storage, or the configured defaults if the system has
not been started. See [The Integrant system](https://git.dgknght.com/dgknght/clj-money/wiki/Integrant-System) for the system's
components and how they depend on each other.

Compile and watch sass:

```bash
sass --watch src/scss/site.scss resources/public/css/site.css
```

Start the ClojureScript frontend:

```bash
lein fig:build
```

Stop the client:

```clojure
:cljs/quit
```

### Local HTTPS with Caddy (optional)

A `Caddyfile` in the project root fronts the app with
[Caddy](https://caddyserver.com/) as a local reverse proxy, so it's available
at `https://money.localhost` with an automatically trusted certificate
instead of the raw `http://lvh.me:3000` address. This is optional — it's a
convenience for testing things that behave differently over HTTPS, not
required for day-to-day development. `caddy` is pinned in `mise.toml`, so
`mise install` picks it up along with the other project tools — no separate
install step needed.

Caddy binds to port 443, which on Linux requires elevated privileges. Rather
than running Caddy itself as root, grant just that capability to the binary
once:

```bash
mise run caddy-setcap
```

(This needs to be re-run any time the pinned `caddy` version in
`mise.toml` changes, since the capability is tied to that specific binary.)

With the app server running (`(go)`, above), start Caddy from the
project root:

```bash
mise run caddy
```

Then browse to [https://money.localhost](https://money.localhost).

## Lein tasks

The lein aliases in `project.clj` (e.g. `lein re-index`, `lein purge-entity`)
run functions in `clj-money.tasks` and elsewhere. A task that needs
application components should start only those parts of the system with
`clj-money.system/with-system`, which binds them while the body runs and
halts them (closing connection pools) afterward:

```clojure
(system/with-system [sys [:clj-money.db/storage]]
  (entities/select {:entity/name "Personal"}))
```

The SQL migration, database creation and partitioning tasks, and the Datomic
schema task, connect with the DDL or admin credentials on their own instead.
See [The Integrant system](https://git.dgknght.com/dgknght/clj-money/wiki/Integrant-System#lein-tasks).

## Dependency Updates

Dependencies are kept current via [Renovate](https://github.com/apps/renovate). It is configured in `renovate.json` to open weekly PRs (Monday mornings, America/Chicago) covering:

- Clojure dependencies (`project.clj`) — grouped into a single PR
- Tool versions (`mise.toml`) — grouped into a single PR
- Docker image versions (`docker-compose.yaml`) — one PR per image

To activate Renovate on a new fork or installation, install the [Renovate GitHub App](https://github.com/apps/renovate) and grant it access to this repository.

## OpenTelemetry

The OTEL Java agent is downloaded by `mise run setup`. To use it:

```bash
lein with-profile +otel repl
```

Then start the system as usual with `(go)`.

## Claude

Claude needs some access tokens to work

- `CLAUDE_FORGEJO_ACCESS_TOKEN` to create PRs, fix build errors, etc.
- `HONEYBADGER_ACCESS_TOKEN` to resolve production and staging errors.
