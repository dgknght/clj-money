# Running with Docker (Podman)

Create a `.env` file in the project root (see `.env` for an example) with at
minimum:

```bash
REDIS_PASSWORD=...
SQL_ADM_USER=...
SQL_ADM_PASSWORD=...
SQL_DDL_USER=...
SQL_DDL_PASSWORD=...
SQL_APP_USER=...
SQL_APP_PASSWORD=...
SQL_DB_NAME=...
SQL_HOST=sql
DATOMIC_DB_NAME=...
SESSION_SECRET=...
```

Create `env/docker/config.edn` (this file is gitignored, so credentials are
safe). Use the structure below, substituting your own credential values. Host
names (`sql`, `redis`) are the Docker Compose service names and should not
change.

```edn
{:application-name "clj-money"
 :alpha-vantage-key "<alpha-vantage-key-here>"
 :db {:strategies {:sql
                   {:clj-money.db/strategy :clj-money.db/sql
                    :host :config/sql-host
                    :port 5432
                    :user :config/sql-app-user
                    :password :config/sql-app-password
                    :dbtype "postgresql"
                    :dbname :config/sql-db-name}

                   :datomic-peer
                   {:clj-money.db/strategy :clj-money.db/datomic-peer
                    :uri "datomic:sql://money_development?jdbc:postgresql://localhost:5432/datomic?user=app_user&password=please01"}}
      :active :datomic-peer}
 :dev? true
 :detailed-import-logging true
 :google-client-id ""
 :google-client-secret ""
 :image-storage {:clj-money.images/strategy :clj-money.images/sql
                 :host :config/sql-host
                 :port 5432
                 :user :config/sql-app-user
                 :password :config/sql-app-password
                 :dbtype "postgresql"
                 :dbname :config/sql-db-name}
 :mailer-enabled? true
 :mailer-from "no-reply@clj-money.com"
 :mailer-host "localhost"
 :partition-period :year
 :progress {:strategies {:redis {:clj-money.progress/strategy :clj-money.progress/redis
                                 :prefix "docker"
                                 :redis-config {:host "redis"
                                                :port 6379
                                                :password :config/redis-password}}}
            :active :redis}
 :redis-password "<redis-password>"
 :secret "dev secret"
 :session-secret "session secret"
 :show-error-messages? true
 :site-host "localhost:3000"
 :site-protocol "http"
 :sql-adm-user "<sql-adm-user>"
 :sql-adm-password "<sql-adm-password>"
 :sql-app-user "<sql-app-user>"
 :sql-app-password "<sql-app-password>"
 :sql-db-name "money_development"
 :sql-ddl-user "<sql-ddl-user>"
 :sql-ddl-password "<sql-ddl-password>"
 :sql-host "localhost"}
```

When using the `datomic-peer` profile, also create
`env/docker/transactor.properties`.

```properties
host=0.0.0.0
alt-host=datomic-transactor
port=4334

ping-host=0.0.0.0
ping-port=9999

protocol=sql
sql-user=app_user
sql-password=please01
sql-url=jdbc:postgresql://sql:5432/datomic
sql-driver-class=org.postgresql.Driver

memcached=memcached:11211
memcached-auto-discovery=false

memory-index-threshold=32m
memory-index-max=512m
object-cache-max=128m
```

Then bring up the stack with the desired profile:

Basic

```bash
podman-compose up
```

Datomic Peer

```bash
podman-compose --profile datomic-peer up
```

Then run the app as described in [Development mode](development.md).

## Upgrading PostgreSQL (major version)

These steps preserve existing development data across a major PostgreSQL version
upgrade. The example below upgrades from 17 to 18, but the same procedure
applies to any major version bump.

### 1. Update `docker-compose.yaml`

- Change the `sql` service image (e.g. `postgres:17` → `postgres:18`)
- Rename the volume to match the new version (e.g. `sql-data-17` → `sql-data-18`)
  in both the top-level `volumes:` block and the `sql` service's `volumes:` list
- Update the mount point to `/var/lib/postgresql` (PostgreSQL 18+ stores data
  in a version-specific subdirectory inside this path; prior versions used
  `/var/lib/postgresql/data`)

### 2. Export data from the running container

Load credentials from `.env`, then dump all databases and roles:

```bash
export $(grep -v '^#' .env | xargs)
pg_dumpall -h localhost -p 5432 -U $SQL_ADM_USER > /tmp/pg_dump.sql
```

### 3. Replace the sql container (leave other services running)

Stop and remove the sql container and its direct dependents (pgadmin), leaving
datomic and other services untouched:

```bash
podman stop clj-money_pgadmin_1 clj-money_sql_1
podman rm clj-money_pgadmin_1 clj-money_sql_1
```

Start the new sql container (picks up the updated image and volume from step 1):

```bash
podman-compose up -d sql
```

### 4. Restore the data

*This will need the same env vars sourced earlier.*

```bash
psql -h localhost -p 5432 -U $SQL_ADM_USER -d postgres < /tmp/pg_dump.sql
```

### 5. Restart dependent services

```bash
podman-compose up -d pgadmin
```

The old volume (e.g. `sql-data-17`) is left on disk. Once you are satisfied
with the upgrade you can remove it:

```bash
podman volume rm clj-money_sql-data-17
```
