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

Then start/stop the server from the REPL:

```clojure
(start-server)
(stop-server)
```

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

## OpenTelemetry

The OTEL Java agent is downloaded by `mise run setup`. To use it:

```bash
lein with-profile +otel repl
```

Then start the server as usual with `(start-server)`.
