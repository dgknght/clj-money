(ns clj-money.repl
  (:require [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io]
            [clj-money.web.server :as s]
            [clj-money.entities :as entities]
            [clj-money.util :as util]
            [clj-money.entities.propagation :as prop]
            [clj-money.entities.transactions :as trx]
            [clj-money.entities.prices :as prices]))

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
        entities/validate
        entities/put)
    (catch Exception e
      (println "Unable to save the user: " (ex-message e))
      (when-let [data (ex-data e)]
        (pprint data)))))

(defn set-password
  [& {:keys [email password]}]
  (-> (entities/find-by {:user/email email})
      (assoc :user/password password)
      entities/put))

(defn propagate-all
  [entity-name]
  (prop/propagate-all (entities/find-by {:entity/name entity-name})
                      {}))

(defn- find-account
  [names entity]
  (entities/find-by (cond-> {:account/name (last names)
                           :account/entity entity}
                    (< 1 (count names)) (assoc :account/parent
                                               (find-account (butlast names) entity)))))

(defn propagate-account
  [entity-name  & account-names]
  (let [entity (entities/find-by {:entity/name entity-name})]
    (trx/propagate-account-from-start
      entity
      (find-account account-names entity))))

(defn propagate-prices
  [entity-name]
  (prices/propagate-all (entities/find-by {:entity/name entity-name})
                        {}))

(defn parse-performance-logs
  [path]
  (with-open[reader (io/reader path)]
    (->> (line-seq reader)
         (map (partial re-find #"(?<=^.*\[performance\] ).*"))
         (filter identity)
         (map read-string)
         (into []))))

(defn stack-includes?
  ([pattern]
   (fn [datum]
     (stack-includes? datum pattern)))
  ([{:keys [stack]} pattern]
   (some (partial re-find pattern) stack)))

(defn sort-by-count
  [data]
  (->> data
       (group-by :query)
       (map #(update-in % [1] (fn [data]
                                {:count (count data)
                                 :stacks (->> data
                                              (map :stack)
                                              frequencies)})))
       (sort-by (comp :count second) >)))

(defn sort-by-average
  [data]
  (->> data
       (group-by :query)
       (map #(update-in % [1] (fn [data]
                                {:average (/ (->> data (map :millis) (reduce +))
                                             (count data))
                                 :stacks (->> data
                                              (map :stack)
                                              frequencies)})))
       (sort-by (comp :average second) >)))
