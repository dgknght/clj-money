(ns clj-money.api.reports
  (:require [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.api :as api]
            [clj-money.util :as util]
            [clj-money.authorization :as auth :refer [authorize]]
            [clj-money.authorization.reports]
            [clj-money.dates :as dates]
            [clj-money.entities :as entities]
            [clj-money.reports.portfolio :as portfolio]
            [clj-money.monitor-cache :as monitor-cache]
            [clj-money.report-cache :as report-cache]
            [clj-money.web.system :as system])
  (:import java.math.BigDecimal
           clojure.lang.Ratio))

; Running a report takes the :report permission on the entity, whatever
; the user may do with the entity or the budget the report is run on
(defn- authorize-report
  [entity-ref user]
  ; not tagged with util/entity-type, which only takes stored types; the
  ; :report type is read from the key
  (authorize {:report/entity entity-ref} ::auth/show user))

(defn- fetch-entity
  [{:keys [params authenticated]}]
  (when-let [entity (entities/find-by
                      (util/entity-type {:id (:entity-id params)} :entity))]
    (authorize-report entity authenticated)
    entity))

(defn- income-statement
  [{{:keys [since as-of]} :params :as req}]
  (if-let [entity (fetch-entity req)]
    (api/response
      (report-cache/income-statement (system/component req :report-cache)
                                     entity
                                     (dates/unserialize-local-date since)
                                     (dates/unserialize-local-date as-of)))
    api/not-found))

(defn- balance-sheet
  [{:keys [params] :as req}]
  (if-let [entity (fetch-entity req)]
    (api/response
      (report-cache/balance-sheet (system/component req :report-cache)
                                  entity
                                  (dates/unserialize-local-date (:as-of params))))
    api/not-found))

(defn- portfolio-params
  [entity {:keys [params]}]
  (-> params
      (select-keys [:aggregate :as-of])
      (update-in-if [:aggregate] keyword)
      (update-in-if [:as-of] dates/unserialize-local-date)
      (assoc :entity entity)))

(defn- portfolio
  [req]
  (or (some-> (fetch-entity req)
              (portfolio-params req)
              portfolio/report
              api/response)
      api/not-found))

(defn- budget-opts
  [params]
  (-> params
      (select-keys [:as-of :tags])
      (update-in-if [:tags] #(mapv keyword %))
      (update-in-if [:as-of] dates/unserialize-local-date)))

(defn- budget
  [{:keys [params authenticated] :as req}]
  (if-let [bdg (entities/find-by
                 (util/entity-type {:id (:budget-id params)} :budget))]
    (do
      (authorize-report (:budget/entity bdg) authenticated)
      (api/response
      (report-cache/budget (system/component req :report-cache)
                           bdg
                           (budget-opts params))))
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
    (if-let [refs (seq (get-in entity
                               [:entity/settings
                                :settings/monitored-accounts]))]
      (->> (entities/select (util/entity-type {:id [:in (mapv :id refs)]}
                                              :account))
           (map (comp serialize-monitor
                      (partial monitor-cache/fetch
                               (system/component req :monitor-cache))))
           api/response)
      (api/response []))
    api/not-found))

(def routes
  [["entities/:entity-id/reports"
    ["/income-statement/:since/:as-of" {:get {:handler income-statement}}]
    ["/balance-sheet/:as-of" {:get {:handler balance-sheet}}]
    ["/budget-monitors" {:get {:handler monitors}}]
    ["/portfolio" {:get {:handler portfolio}}]]
   ["reports/budget/:budget-id" {:get {:handler budget}}]])
