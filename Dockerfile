FROM clojure:lein-2.10.0-jammy AS psql
RUN <<EOF
apt-get update
apt-get install -y postgresql-client
EOF

FROM psql AS util
RUN mkdir -p /usr/src/clj-money
WORKDIR /usr/src/clj-money
COPY . .

FROM util AS build
RUN <<EOF
apt-get install --yes nodejs npm
npm install --global sass
EOF

# these can be anything to pass the validation in the library
# The correct values will be supplied to the container
ENV GOOGLE_OAUTH_CLIENT_ID=build-client-id
ENV GOOGLE_OAUTH_CLIENT_SECRET=build-client-secret
RUN <<EOF
mkdir resources/public/css
sass src/scss/site.scss resources/public/css/site.css
lein do fig:prod, uberjar
EOF

FROM eclipse-temurin:8-jre-noble as web
WORKDIR /opt/clj-money
COPY --from=build /usr/src/clj-money/target/clj-money.jar .
CMD ["java", "-cp", "clj-money.jar", "clojure.main", "-m", "clj-money.server"]

# Default port for the service
EXPOSE 3000
