# clj-money
Clojure cloud accounting application

![build status](https://travis-ci.org/dgknght/clj-money.svg?branch=master)

## Running locally

In a terminal:
```bash
lein repl
```

Inside the repl:
```
(require 'clj-money.web)
(def server (clj-money.web/-main))
```

To stop
```
(.stop server)
```

To start again
```
(.start server)
```

## Running on Heroku
```
heroku create my-app-name
heroku addons:create heroku-postgres:hobby-dev
heroku config:set DB=<value of DATABASE_URL> PARTITION_PERIOD=year
git push heroku master
heroku run lein migrate
heroku run lein partition <start-date> <end-date>
heroku open

## License
Distributed under the Eclipse Public License, the same as Clojure.
