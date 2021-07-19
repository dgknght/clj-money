(ns clj-money.api.reports
  (:require [lambdaisland.uri :refer [map->query-string uri]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.web :refer [serialize-date]]
            [clj-money.state :refer [current-entity]]
            [dgknght.app-lib.api :as api]))

(defn- after-read
  [report]
  (map (fn [item]
         (-> item
             (update-in [:style] keyword)
             (update-in-if [:items] after-read)))
       report))

(defn income-statement
  [{:keys [start-date end-date]} success-fn error-fn]
  (api/get (api/path :entities
                     (:id @current-entity)
                     :reports
                     :income-statement
                     (serialize-date start-date)
                     (serialize-date end-date))
           (comp success-fn after-read)
           error-fn))

(defn balance-sheet
  [{:keys [as-of]} success-fn error-fn]
  (api/get (api/path :entities
                     (:id @current-entity)
                     :reports
                     :balance-sheet
                     (serialize-date as-of))
           (comp success-fn after-read)
           error-fn))

(defn budget
  [{:keys [budget-id] :as opts} success-fn error-fn]
  (let [url (str
              (assoc
                (uri (api/path :reports
                               :budget
                               budget-id))
                :query (-> opts
                           (dissoc :budget-id)
                           (update-in-if [:tags] #(map name %))
                           map->query-string)))]

    (.log js/console url)

    (api/get url
             (comp success-fn #(update-in % [:items] after-read))
             error-fn)))

(defn budget-monitors
  [success-fn error-fn]
  (api/get (api/path :entities
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
   (api/get (str (api/path :entities
                           (:id @current-entity)
                           :reports
                           :portfolio)
                 "?"
                 (-> options
                     (update-in-if [:as-of] serialize-date)
                     (update-in-if [:aggregate] name)
                     map->query-string))
            (comp success-fn
                  after-portfolio-read)
            error-fn)))
