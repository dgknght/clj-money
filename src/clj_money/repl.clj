(ns clj-money.repl
  (:require [clojure.pprint :refer [pprint]]
            [clj-money.web.server :as s]
            [clj-money.models :as models]
            [clj-money.util :as util]
            [clj-money.models.propagation :as prop]
            [clj-money.models.transactions :as trx]
            [clj-money.models.prices :as prices]))

(def server (atom nil))

(defn start-server []
  (reset! server (s/-main)))

(defn stop-server []
  (.stop @server)
  (reset! server nil))

(defn create-user
  [& {:as params}]
  (try
    (-> params
        (select-keys [:first-name
                      :last-name
                      :email
                      :password])
        (util/qualify-keys :user)
        models/validate
        models/put)
    (catch Exception e
      (println "Unable to save the user: " (ex-message e))
      (when-let [data (ex-data e)]
        (pprint data)))))

^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn set-password
  [& {:keys [email password]}]
  (-> (models/find-by {:user/email email})
      (assoc :user/password password)
      models/put))

^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn propagate-all
  [entity-name]
  (prop/propagate-all (models/find-by {:entity/name entity-name})
                      {}))

(defn- find-account
  [names entity]
  (models/find-by (cond-> {:account/name (last names)
                           :account/entity entity}
                    (< 1 (count names)) (assoc :account/parent
                                               (find-account (butlast names) entity)))))

^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn propagate-account
  [entity-name  & account-names]
  (let [entity (models/find-by {:entity/name entity-name})]
    (trx/propagate-account-from-start
      entity
      (find-account account-names entity))))

^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn propagate-prices
  [entity-name]
  (prices/propagate-all (models/find-by {:entity/name entity-name})
                        {}))
