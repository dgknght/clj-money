(ns clj-money.api.reports
  (:require [compojure.core :refer [defroutes GET]]
            [clj-money.util :refer [unserialize-date
                                    update-in-if]]
            [clj-money.api :as api]
            [clj-money.authorization :refer [+scope]]
            [clj-money.models :as models]
            [clj-money.models.entities :as entities]
            [clj-money.models.budgets :as budgets]
            [clj-money.models.accounts :as accounts]
            [clj-money.reports :as rpt])
  (:import java.math.BigDecimal
           clojure.lang.Ratio))

(defn- fetch-entity
  [{:keys [params authenticated]}]
  (entities/find-by (+scope {:id (:entity-id params)}
                            ::models/entity
                            authenticated)))

(defn- income-statement
  [{{:keys [start-date end-date]} :params :as req}]
  (if-let [entity (fetch-entity req)]
    (api/->response
     (rpt/income-statement entity
                           (unserialize-date start-date)
                           (unserialize-date end-date)))
    (api/not-found)))

(defn- balance-sheet
  [{:keys [params] :as req}]
  (if-let [entity (fetch-entity req)]
    (api/->response
     (rpt/balance-sheet entity
                        (unserialize-date (:as-of params))))
    (api/not-found)))

(defn- portfolio
  [{:keys [params] :as req}]
  (if-let [entity (fetch-entity req)]
    (api/->response (rpt/portfolio (-> params
                                       (select-keys [:aggregate :as-of])
                                       (update-in-if [:aggregate] keyword)
                                       (update-in-if [:as-of] unserialize-date)
                                       (assoc :entity entity))))
    (api/not-found)))

(defn- budget
  [{:keys [params authenticated]}]
  (if-let [budget (budgets/find-by  (+scope {:id (:budget-id params)}
                                            ::models/budget
                                            authenticated))]
    (api/->response
     (rpt/budget budget (-> params
                            (select-keys [:as-of])
                            (update-in-if [:as-of] unserialize-date))))
    (api/not-found)))

(defn- flatten-ratio
  [ratio]
  (if (instance? Ratio ratio)
    (.divide (bigdec (numerator ratio))
             (bigdec (denominator ratio))
             4
             BigDecimal/ROUND_HALF_UP)
    ratio))

(defn- serialize-monitor
  [monitor]
  (-> monitor
      (update-in [:budget :percentage] flatten-ratio)
      (update-in [:period :percentage] flatten-ratio)
      (assoc :account-id (-> monitor :account :id))
      (dissoc :account)))

(defn- monitors
  [req]
  (if-let [entity (fetch-entity req)]
    (api/->response (map (comp serialize-monitor
                               rpt/monitor
                               accounts/find)
                         (get-in entity [:settings :monitored-account-ids])))
    (api/not-found)))

(defroutes routes
  (GET "/api/entities/:entity-id/reports/income-statement/:start-date/:end-date"
    req
    (income-statement req))
  (GET "/api/entities/:entity-id/reports/balance-sheet/:as-of"
    req
    (balance-sheet req))
  (GET "/api/entities/:entity-id/reports/budget-monitors"
    req
    (monitors req))
  (GET "/api/entities/:entity-id/reports/portfolio"
    req
    (portfolio req))
  (GET "/api/reports/budget/:budget-id"
    req
    (budget req)))
