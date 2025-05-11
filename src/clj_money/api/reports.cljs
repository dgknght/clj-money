(ns clj-money.api.reports
  (:require [lambdaisland.uri :refer [map->query-string uri]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.dates :refer [serialize-local-date]]
            [clj-money.api :as api :refer [add-error-handler]]
            [clj-money.state :refer [current-entity]]))

(defn income-statement
  [{:keys [start-date end-date]} & {:as opts}]
  (api/get (api/path :entities
                     (:id @current-entity)
                     :reports
                     :income-statement
                     (serialize-local-date start-date)
                     (serialize-local-date end-date))
           {}
           (add-error-handler opts "Unable to retrieve the income statement: %s")))

(defn balance-sheet
  [{:keys [as-of]} & {:as opts}]
  (api/get (api/path :entities
                     (:id @current-entity)
                     :reports
                     :balance-sheet
                     (serialize-local-date as-of))
           {}
           (add-error-handler opts "Unable to retrieve the balance sheet: %s")))

(defn- budget-report-url
  [{:keys [budget-id] :as report}]
  (-> (api/path :reports
                :budget
                budget-id)
      uri
      (assoc :query (-> report
                        (dissoc :budget-id)
                        (update-in-if [:tags] #(map name %))
                        map->query-string))
      str))

(defn budget
  [report & {:as opts}]
  (api/get (budget-report-url report)
           {}
           (add-error-handler opts "Unable to retrieve the budget report: %s")))

(defn budget-monitors
  [& {:as opts}]
  (api/get (api/path :entities
                     @current-entity
                     :reports
                     :budget-monitors)
           {}
           (add-error-handler opts "Unable to retrieve the budget monitors: %s")))

(def ^:private default-portfolio-report
  {:aggregate :by-account})

(defn- portfolio-report-url
  [report]
  (str (api/path :entities
                 (:id @current-entity)
                 :reports
                 :portfolio)
       "?"
       (-> report
           (update-in-if [:as-of] serialize-local-date)
           (update-in-if [:aggregate] name)
           map->query-string)))

(defn portfolio
  [report & {:as opts}]
  (-> default-portfolio-report
      (merge report)
      portfolio-report-url
      (api/get {}
               (add-error-handler opts "Unable to retrieve the portfolio report: %s"))))
