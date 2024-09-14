(ns clj-money.api.reports
  (:require [lambdaisland.uri :refer [map->query-string uri]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.web :refer [serialize-date]]
            [dgknght.app-lib.api-async :as api]
            [clj-money.api :refer [handle-ex]]
            [clj-money.state :refer [current-entity]]))

(defn- after-read
  [report]
  (map (fn [item]
         (-> item
             (update-in [:style] keyword)
             (update-in-if [:items] after-read)))
       report))

(defn income-statement
  [{:keys [start-date end-date]} & xf]
  (api/get (api/path :entities
                     (:id @current-entity)
                     :reports
                     :income-statement
                     (serialize-date start-date)
                     (serialize-date end-date))
           {}
           {:transform (apply comp
                              (map after-read)
                              xf)
            :handle-ex (handle-ex "Unable to retrieve the income statement: %s")}))

(defn balance-sheet
  [{:keys [as-of]} & xf]
  (api/get (api/path :entities
                     (:id @current-entity)
                     :reports
                     :balance-sheet
                     (serialize-date as-of))
           {}
           {:transform (cons (map after-read) xf)
            :handle-ex (handle-ex "Unable to retrieve the balance sheet: %s")}))

(defn budget
  [{:keys [budget-id] :as opts} & xf]
  (let [url (str
              (assoc
                (uri (api/path :reports
                               :budget
                               budget-id))
                :query (-> opts
                           (dissoc :budget-id)
                           (update-in-if [:tags] #(map name %))
                           map->query-string)))]
    (api/get url
             {}
             {:transform (cons (map #(update-in % [:items] after-read))
                               xf)
              :handle-ex (handle-ex "Unable to retrieve the budget report: %s")})))

(defn budget-monitors
  [& xf]
  (api/get (api/path :entities
                     (:id @current-entity)
                     :reports
                     :budget-monitors)
           {}
           {:transform xf
            :handle-ex (handle-ex "Unable to retrieve the budget monitors: %s")}))

(defn- after-portfolio-read
  [report]
  (map #(update-in-if % [:parents] set) report))

(defn portfolio
  ([xf]
   (portfolio {:aggregate :by-account} xf))
  ([options xf & xfs]
   (api/get (str (api/path :entities
                           (:id @current-entity)
                           :reports
                           :portfolio)
                 "?"
                 (-> options
                     (update-in-if [:as-of] serialize-date)
                     (update-in-if [:aggregate] name)
                     map->query-string))
            {}
            {:transform (cons (map after-portfolio-read)
                              (cons xf xfs))
             :handle-ex (handle-ex "Unable to retrieve the portfolio report: %s")})))
