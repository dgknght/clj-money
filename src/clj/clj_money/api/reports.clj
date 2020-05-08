(ns clj-money.api.reports
  (:require [compojure.core :refer [defroutes GET]]
            [environ.core :refer [env]]
            [clj-money.x-platform.util :refer [unserialize-date
                                               update-in-if]]
            [clj-money.api :as api]
            [clj-money.authorization :refer [+scope]]
            [clj-money.models :as models]
            [clj-money.models.entities :as entities]
            [clj-money.models.budgets :as budgets]
            [clj-money.reports :as rpt]))

(defn- fetch-entity
  [{:keys [params authenticated]}]
  (entities/find-by (env :db) (+scope {:id (:entity-id params)}
                                      ::models/entity
                                      authenticated)))

(defn- income-statement
  [{{:keys [start-date end-date]} :params :as req}]
  (if-let [entity (fetch-entity req)]
    (api/->response
      (rpt/income-statement (env :db)
                            entity
                            (unserialize-date start-date)
                            (unserialize-date end-date)))
    (api/not-found)))

(defn- balance-sheet
  [{:keys [params] :as req}]
  (if-let [entity (fetch-entity req)]
    (api/->response
      (rpt/balance-sheet (env :db)
                         entity
                         (unserialize-date (:as-of params))))
    (api/not-found)))

(defn- budget
  [{:keys [params authenticated]}]
  (if-let [budget (budgets/find-by (env :db) (+scope {:id (:budget-id params)}
                                                     ::models/budget
                                                     authenticated))]
    (api/->response
      (rpt/budget (env :db) budget (-> params
                                       (select-keys [:as-of])
                                       (update-in-if [:as-of] unserialize-date))))
    (api/not-found)))

(defroutes routes
  (GET "/api/entities/:entity-id/reports/income-statement/:start-date/:end-date"
       req
       (income-statement req))
  (GET "/api/entities/:entity-id/reports/balance-sheet/:as-of"
       req
       (balance-sheet req))
  (GET "/api/reports/budget/:budget-id"
       req
       (budget req)))
