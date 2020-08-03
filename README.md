# clj-money
Clojure cloud accounting application

![build status](https://travis-ci.org/dgknght/clj-money.svg?branch=master)

## Running locally

In a terminal:
```bash
lein repl
```

Inside the repl:
```clojure
(require 'clj-money.web)
(def server (clj-money.web/-main))
```

To stop
```clojure
(.stop server)
```

To start again
```clojure
(.start server)
```

## Running on Heroku
```bash
heroku create my-app-name
heroku addons:create heroku-postgres:hobby-dev
heroku config:set DB=<value of DATABASE_URL> PARTITION_PERIOD=year
git push heroku master
heroku run lein migrate
heroku run lein partition <start-date> <end-date>
heroku open
```

## Running tests
```bash
createdb money_test
lein with-profile test migrate
lein with-profile test partition 2015-01-01 2017-12-31
lein test
```

## License
Distributed under the Eclipse Public License, the same as Clojure.
