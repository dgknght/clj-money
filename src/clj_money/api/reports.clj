(ns clj-money.api.reports
  (:require [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.api :as api]
            [dgknght.app-lib.authorization :refer [+scope]]
            [clj-money.dates :as dates]
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
    (api/response
      (rpt/income-statement entity
                            (dates/unserialize-local-date start-date)
                            (dates/unserialize-local-date end-date)))
    api/not-found))

(defn- balance-sheet
  [{:keys [params] :as req}]
  (if-let [entity (fetch-entity req)]
    (api/response
      (rpt/balance-sheet entity
                         (dates/unserialize-local-date (:as-of params))))
    api/not-found))

(defn- portfolio
  [{:keys [params] :as req}]
  (if-let [entity (fetch-entity req)]
    (api/response (rpt/portfolio (-> params
                                     (select-keys [:aggregate :as-of])
                                     (update-in-if [:aggregate] keyword)
                                     (update-in-if [:as-of] dates/unserialize-local-date)
                                     (assoc :entity entity))))
    api/not-found))

(defn- budget
  [{:keys [params authenticated]}]
  (if-let [budget (budgets/find-by  (+scope {:id (:budget-id params)}
                                            ::models/budget
                                            authenticated))]
    (api/response
      (rpt/budget budget (-> params
                             (select-keys [:as-of :tags])
                             (update-in-if [:tags] #(mapv keyword %))
                             (update-in-if [:as-of] dates/unserialize-local-date))))
    api/not-found))

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
    (api/response (map (comp serialize-monitor
                             rpt/monitor
                             accounts/find)
                       (get-in entity [:settings :monitored-account-ids])))
    api/not-found))

(def routes
  [["entities/:entity-id/reports"
    ["/income-statement/:start-date/:end-date" {:get {:handler income-statement}}]
    ["/balance-sheet/:as-of" {:get {:handler balance-sheet}}]
    ["/budget-monitors" {:get {:handler monitors}}]
    ["/portfolio" {:get {:handler portfolio}}]]
   ["reports/budget/:budget-id" {:get {:handler budget}}]])
