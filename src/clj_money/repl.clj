(ns clj-money.repl
  (:require [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [reitit.core :as reitit]
            [integrant.core :as ig]
            ; referred for use at the REPL
            #_{:clj-kondo/ignore [:unused-referred-var]}
            [integrant.repl :refer [go halt reset]]
            [integrant.repl.state :as state]
            [clj-money.web.handler :as h]
            [clj-money.system :as system]
            [clj-money.entities :as entities]
            [clj-money.util :as util]
            [clj-money.ingestion :as ing]
            [clj-money.ingestion.ref]
            [clj-money.db :as db]
            [clj-money.entities.attachments :as atts]
            [clj-money.entities.propagation :as prop]
            [clj-money.entities.transactions :as trx]
            [clj-money.entities.prices :as prices]))

(defn print-routes []
  (doseq [[method path handler]
          (->> (reitit/compiled-routes (h/router))
               (mapcat (fn [[path opts]]
                         (->> [:get :post :put :patch :delete]
                              (map (juxt identity opts))
                              (filter (comp identity second))
                              (map (fn [[method opts]]
                                     [method path (:handler opts)]))))))]
    (println (string/upper-case (name method))
             path
             "->"
             (class handler))))

; Use (go), (halt) and (reset) from integrant.repl to manage the system
(integrant.repl/set-prep!
  (fn []
    (doto (system/config)
      ig/load-namespaces)))

(defmacro ^:private with-system
  "Evaluates body with the components of the running system bound, if the
  system has been started with (go)."
  [& body]
  `(system/with-components state/system ~@body))

(defn create-user
  [& {:as params}]
  (with-system
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
          (pprint data))))))

(defn set-password
  [& {:keys [email password]}]
  (with-system
    (-> (entities/find-by {:user/email email})
        (assoc :user/password password)
        entities/put)))

(defn propagate-all
  [entity-name]
  (with-system
    (prop/propagate-all (entities/find-by {:entity/name entity-name})
                        {})))

(defn- find-account
  [names entity]
  (entities/find-by (cond-> {:account/name (last names)
                           :account/entity entity}
                    (< 1 (count names)) (assoc :account/parent
                                               (find-account (butlast names) entity)))))

(defn propagate-account
  [entity-name  & account-names]
  (with-system
    (let [entity (entities/find-by {:entity/name entity-name})]
      (trx/propagate-account-from-start
        entity
        (find-account account-names entity)))))

(defn propagate-prices
  [entity-name]
  (with-system
    (prices/propagate-all (entities/find-by {:entity/name entity-name})
                          {})))

(defn propagate-attachments
  [entity-name]
  (with-system
    (atts/propagate-all (entities/find-by {:entity/name entity-name})
                        {})))

(defn purge-entity
  "Completely and permanently removes the named entity and everything that
  depends on it. This is irreversible."
  [{:keys [entity-name user-email]}]
  (with-system
    (if-let [e (entities/find-by {:entity/name entity-name
                                  :user/email user-email}
                                 {:type :entity})]
      (entities/purge! e)
      (println (format "Unable to find an entity named \"%s\" for user \"%s\""
                       entity-name
                       user-email)))))

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

(defn read-receipt
  [source user-email entity-name & {:as opts}]
  (system/with-system [sys [::ing/reader ::db/storage]]
    (let [{::ing/keys [reader]} sys
          entity (entities/find-by {:entity/name entity-name
                                    :user/email user-email}
                                   {:entity-type :entity})
          _ (assert entity
                    (format "No entity named %s for user %s"
                            (pr-str entity-name)
                            (pr-str user-email)))
          result (try
                   (ing/read-receipt reader source entity opts)
                   (catch Exception e
                     {:type (type e)
                      :message (ex-message e)
                      :data (ex-data e)
                      :stack (mapv str (.getStackTrace e))}))]
      (pprint result))))
