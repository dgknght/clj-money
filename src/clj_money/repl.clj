(ns clj-money.repl
  (:require [clj-money.web.server :as s]
            [clj-money.models :as models]
            [clj-money.models.propagation :as prop]
            [clj-money.models.prices :as prices]))

(def server (atom nil))

(defn start-server []
  (reset! server (s/-main)))

(defn stop-server []
  (.stop @server)
  (reset! server nil))

(defn create-user
  [& {:keys [first-name last-name email password]}]
  (models/put #:user{:first-name first-name
                     :last-name last-name
                     :email email
                     :password password}))

^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn propagate-all
  [entity-name]
  (prop/propagate-all (models/find-by {:entity/name entity-name})))

^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn propagate-prices
  [entity-name]
  (prices/propagate-all (models/find-by {:entity/name entity-name})))
