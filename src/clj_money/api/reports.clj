(ns clj-money.api.reports
  (:require [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.api :as api]
            [clj-money.util :as util]
            [clj-money.authorization :refer [+scope]]
            [clj-money.dates :as dates]
            [clj-money.models :as models]
            [clj-money.reports :as rpt])
  (:import java.math.BigDecimal
           clojure.lang.Ratio))

(defn- fetch-entity
  [{:keys [params authenticated]}]
  (-> {:id (:entity-id params)}
      (util/model-type :entity)
      (+scope :entity authenticated)
      models/find-by))

(defn- income-statement
  [{{:keys [since as-of]} :params :as req}]
  (or (some-> (fetch-entity req)
              (rpt/income-statement (dates/unserialize-local-date since)
                                    (dates/unserialize-local-date as-of))
              api/response)
      api/not-found))

(defn- balance-sheet
  [{:keys [params] :as req}]
  (or (some-> (fetch-entity req)
              (rpt/balance-sheet (dates/unserialize-local-date (:as-of params)))
              api/response)
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
  (or (some-> (models/find-by (+scope {:id (:budget-id params)}
                                      :budget
                                      authenticated))
              (rpt/budget (-> params
                              (select-keys [:as-of :tags])
                              (update-in-if [:tags] #(mapv keyword %))
                              (update-in-if [:as-of] dates/unserialize-local-date)))
              api/response)
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
      (update-in [:report/budget
                  :report/percentage]
                 flatten-ratio)
      (update-in [:report/period
                  :report/percentage]
                 flatten-ratio)))

(defn- monitors
  [req]
  (if-let [entity (fetch-entity req)]
    (api/response (map (comp serialize-monitor
                             rpt/monitor
                             (models/find :account))
                       (get-in entity [:entity/settings :settings/monitored-account-ids])))
    api/not-found))

(def routes
  [["entities/:entity-id/reports"
    ["/income-statement/:since/:as-of" {:get {:handler income-statement}}]
    ["/balance-sheet/:as-of" {:get {:handler balance-sheet}}]
    ["/budget-monitors" {:get {:handler monitors}}]
    ["/portfolio" {:get {:handler portfolio}}]]
   ["reports/budget/:budget-id" {:get {:handler budget}}]])
