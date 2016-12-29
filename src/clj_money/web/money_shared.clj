(ns clj-money.web.money-shared
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [environ.core :refer [env]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [ring.util.response :refer :all]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.reports :as reports])
  (:use [clj-money.web.shared :refer :all]))

(defn account-options
  "Returns a list of account for the specified entity as options ready for
  use with 'select-field'"
  ([entity-id] (account-options entity-id {}))
  ([entity-id options]
   (cond->> entity-id
     true (accounts/select-by-entity-id (env :db))
     (contains? options :types) (filter #(contains? (:types options) (:type %)))
     true (sort-by :name)
     true (map #(select-keys % [:id :name]))
     true (map #(rename-keys % {:id :value
                                :name :caption}))
     (contains? options :include-none?) (concat [{:value "" :caption "None"}]))))

(defn- paced-progress-bar
  [data]
  [:span.budget-monitor {:data-max (:total-budget data)
                             :data-value (:actual data)
                             :data-pacer (:prorated-budget data)}])

(defn- budget-monitor
  [monitor]
  (html
    [:h4 (:caption monitor)]
    (when (:message monitor)
      [:span.note (:message monitor)])
    (when (:period monitor)
      (html
        [:h5 "This Period"]
        (paced-progress-bar (:period monitor))))
    (when (:budget monitor)
      (html
        [:h5 "This Budget"]
        (paced-progress-bar (:budget monitor))))))

(defn budget-monitors
  [entity-id]
  (html
    [:h3 "Budget monitors"]
    (->> ["Groceries"]
         (map (comp #(reports/monitor (env :db) %)
                    #(accounts/find-by-name (env :db) entity-id %)))
         (remove empty?)
         (map budget-monitor))))
