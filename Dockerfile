FROM docker.io/clojure:temurin-25-lein-bookworm-slim AS psql
RUN apt-get update && \
    apt-get install -y postgresql-client

FROM psql AS util
RUN mkdir -p /usr/src/clj-money
WORKDIR /usr/src/clj-money
COPY . .

FROM util AS build
RUN apt-get install --yes nodejs npm && \
    npm install --global sass

RUN mkdir resources/public/css && \
    sass src/scss/site.scss resources/public/css/site.css && \
    lein do fig:prod, uberjar

FROM docker.io/clojure:temurin-25-lein-bookworm-slim AS web
WORKDIR /opt/clj-money
COPY --from=build /usr/src/clj-money/target/clj-money.jar .
CMD ["java", "clojure.main", "-m", "clj-money.web.server"]

# Default port for the service
EXPOSE 3000
