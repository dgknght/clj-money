# clj-money
Clojure cloud accounting application

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

## License
Distributed under the Eclipse Public License, the same as Clojure.
