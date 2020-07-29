(ns clj-money.api.reports
  (:require [clj-money.state :refer [current-entity]]
            [clj-money.api :as api]
            [clj-money.x-platform.util :refer [serialize-date]]))

(defn- after-read
  [report]
  (map #(update-in % [:style] keyword) report))

(defn income-statement
  [{:keys [start-date end-date]} success-fn error-fn]
  (api/get-resource (api/path :entities
                              (:id @current-entity)
                              :reports
                              :income-statement
                              (serialize-date start-date)
                              (serialize-date end-date))
                    (comp success-fn after-read)
                    error-fn))

(defn balance-sheet
  [{:keys [as-of]} success-fn error-fn]
  (api/get-resource (api/path :entities
                              (:id @current-entity)
                              :reports
                              :balance-sheet
                              (serialize-date as-of))
                    (comp success-fn after-read)
                    error-fn))

(defn budget
  [{:keys [budget-id]} success-fn error-fn]
  (api/get-resource (api/path :reports
                              :budget
                              budget-id)
                    (comp success-fn #(update-in % [:items] after-read))
                    error-fn))

(defn budget-monitors
  [success-fn error-fn]
  (api/get-resources (api/path :entities
                               (:id @current-entity)
                               :reports
                               :budget-monitors)
                     success-fn
                     error-fn))
