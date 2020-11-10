(ns clj-money.api.reports
  (:require [clj-money.state :refer [current-entity]]
            [clj-money.api :as api]
            [clj-money.util :refer [serialize-date
                                    map->query-string
                                    update-in-if]]))

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

(defn- after-portfolio-read
  [report]
  (map #(update-in-if % [:parents] set) report))

(defn portfolio
  ([success-fn error-fn]
   (portfolio {:aggregate :by-account} success-fn error-fn))
  ([options success-fn error-fn]
   (api/get-resources (str (api/path :entities
                                (:id @current-entity)
                                :reports
                                :portfolio)
                           "?"
                           (-> options
                               (update-in-if [:as-of] serialize-date)
                               map->query-string))
                      (comp success-fn
                            after-portfolio-read)
                      error-fn)))
