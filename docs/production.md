# Production Configuration

The following configuration values have insecure defaults and **must** be set
to strong, unique values before deploying to a production environment:

| Key | Description |
|-----|-------------|
| `:secret` | JWT signing secret — use a long random string |
| `:session-secret` | Session cookie encryption key — must be exactly 16, 24, or 32 bytes |
| `:google-client-id` | Google OAuth client ID |
| `:google-client-secret` | Google OAuth client secret |
| `:redis-password` | Redis authentication password |
| `:sql-adm-password` | PostgreSQL admin user password |
| `:sql-app-password` | PostgreSQL application user password |
| `:sql-ddl-password` | PostgreSQL DDL user password |
| `:alpha-vantage-key` | Alpha Vantage API key for price data |

These can be supplied via `env/docker/config.edn` or overridden with
environment variables (e.g. `SESSION_SECRET`, `SECRET`, `REDIS_PASSWORD`).

## Image Storage

Set the `:image-storage` key in config to one of the strategies below.

### SQL

Stores image content in the `image_content` table of a PostgreSQL database.

```edn
:image-storage {:clj-money.images/strategy :clj-money.images/sql
                :host "localhost"
                :port 5432
                :dbtype "postgresql"
                :dbname "money_production"
                :user "<app-user>"
                :password "<app-password>"}
```

| Key | Description |
|-----|-------------|
| `:host` | PostgreSQL host |
| `:port` | PostgreSQL port |
| `:dbtype` | Must be `"postgresql"` |
| `:dbname` | Database name |
| `:user` | Database user |
| `:password` | Database password |

### S3

Stores images as objects in an S3 bucket. Compatible with any S3-compatible
service (AWS S3, MinIO, etc.).

```edn
:image-storage {:clj-money.images/strategy :clj-money.images/s3
                :bucket "my-bucket"
                :region "us-east-1"
                :access-key "<aws-access-key>"
                :secret-key "<aws-secret-key>"}
```

| Key | Required | Description |
|-----|----------|-------------|
| `:bucket` | yes | S3 bucket name |
| `:region` | no | AWS region (defaults to `"us-east-1"`) |
| `:access-key` | no | AWS access key ID (omit to use instance/environment credentials) |
| `:secret-key` | no | AWS secret access key (omit to use instance/environment credentials) |
| `:host` | no | Endpoint hostname for S3-compatible services (e.g. MinIO) |
| `:port` | no | Endpoint port, used together with `:host` |
