(ns clj-money.api.scheduled-transactions
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [java-time.api :as t]
            [dgknght.app-lib.api :as api]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.core :refer [parse-bool
                                          update-in-if]]
            [clj-money.authorization :refer [authorize
                                             allowed?
                                             +scope]
             :as authorization]
            [clj-money.dates :as dates]
            [clj-money.util :as util]
            [clj-money.comparatives :as comparatives]
            [clj-money.models :as models]
            [clj-money.models.transactions :refer [with-delayed-propagation]]
            [clj-money.scheduled-transactions :as sched-trans]
            [clj-money.authorization.scheduled-transactions :as sched-trans-auth]))

(defmulti^:private parse-date-param type)

(defmethod parse-date-param ::util/string
  [s]
  (dates/unserialize-local-date s))

(defmethod parse-date-param ::util/vector
  [[oper & vs]]
  (apply vector oper (map parse-date-param vs)))

(defn- ->criteria
  [{:keys [params authenticated]}]
  (-> params
      (rename-keys {:entity-id :entity})
      (util/qualify-keys :scheduled-transaction)
      (update-in [:scheduled-transaction/entity] util/->model-ref)
      (update-in-if [:scheduled-transaction/enabled] parse-bool)
      comparatives/symbolize
      (update-in-if [:scheduled-transaction/end-date] parse-date-param)
      (update-in-if [:scheduled-transaction/start-date] parse-date-param)
      (select-keys [:scheduled-transaction/entity
                    :scheduled-transaction/enabled
                    :scheduled-transaction/end-date
                    :scheduled-transaction/start-date])
      (+scope :scheduled-transaction authenticated)))

(defn- select-options []
  {:nil-replacements
   {:scheduled-transaction/end-date (t/local-date 9999 12 31)
    :scheduled-transaction/start-date (t/minus (t/local-date) (t/days 1))}})

(defn- index
  [req]
  (-> req
      ->criteria
      (models/select (select-options))
      api/response))

(defn- extract-sched-tran
  [{:keys [params]}]
  (select-keys params [:scheduled-transaction/start-date
                       :scheduled-transaction/end-date
                       :scheduled-transaction/enabled
                       :scheduled-transaction/last-occurrence
                       :scheduled-transaction/date-spec
                       :scheduled-transaction/period
                       :scheduled-transaction/entity
                       :scheduled-transaction/description
                       :scheduled-transaction/memo
                       :scheduled-transaction/items]))

(defn- create
  [{:keys [params authenticated] :as req}]
  (-> req
      extract-sched-tran
      (assoc :scheduled-transaction/entity {:id (:entity-id params)})
      (authorize ::authorization/create authenticated)
      models/put
      api/creation-response))

(defn- find-and-authorize
  [{:keys [params authenticated]} action]
  (-> params
      (select-keys [:id])
      (+scope :scheduled-transaction authenticated)
      models/find-by
      (authorize action authenticated)))

(defn- update
  [req]
  (or (some-> (find-and-authorize req ::authorization/update)
              (merge (extract-sched-tran req))
              models/put
              api/response)
      api/not-found))

(defn- delete
  [req]
  (if-let [sched-tran (find-and-authorize req ::authorization/destroy)]
    (do
      (models/delete sched-tran)
      (api/response))
    api/not-found))

(defn- put-many
  [models]
  (if (seq models)
    (with-delayed-propagation [out-chan ctrl-chan]
      (models/put-many {:out-chan out-chan
                        :ctrl-chan ctrl-chan}
                       models))
    []))

(defn- realize
  [req]
  (or (some-> (find-and-authorize req ::sched-trans-auth/realize)
              sched-trans/realize
              put-many
              api/creation-response)
      api/not-found))

(defn- fetch-entity
  [{:keys [params authenticated]}]
  (some-> {:id (:entity-id params)}
          (util/model-type :entity)
          (+scope :entity authenticated)
          models/find-by))

(defn- ready-to-realize
  [req]
  (when-let [entity (fetch-entity req)]
    (models/select
      #:scheduled-transaction{:entity (util/->model-ref entity)
                              :enabled true
                              :start-date [:<= (t/local-date)]
                              :end-date [:>= (t/local-date)]}
      (select-options))))

(defn- mass-realize
  [{:as req :keys [authenticated]}]
  (if-let [ready (ready-to-realize req)]
    (->> ready
         (filter #(allowed? % ::sched-trans-auth/realize authenticated))
         (mapcat sched-trans/realize)
         put-many
         (filter (util/model-type? :transaction))
         (sort-by :transaction/transaction-date t/before?)
         api/creation-response)
    api/not-found))

(def routes
  [["entities/:entity-id/scheduled-transactions"
    ["" {:get {:handler index}
         :post {:handler create}}]
    ["/realize" {:post {:handler mass-realize}}]]
   ["scheduled-transactions/:id"
    ["" {:patch {:handler update}
         :delete {:handler delete}}]
    ["/realize" {:post {:handler realize}}]]])
