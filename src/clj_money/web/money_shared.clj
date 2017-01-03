(ns clj-money.web.money-shared
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [environ.core :refer [env]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [ring.util.response :refer :all]
            [clj-money.util :refer [format-number]]
            [clj-money.inflection :refer [humanize]]
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

(defn- opt-group-for-account-type
  [{account-type :type accounts :accounts} selected-id]
  [:optgroup {:label (humanize account-type)}
   (map #(vector :option
                 (cond-> {:value (:id %)}
                   (= selected-id (:id %)) (assoc :selected true))
                 (:name %))
        accounts)])

(defn grouped-options-for-accounts
  ([entity-id]
   (grouped-options-for-accounts entity-id nil))
  ([entity-id selected-id]
   (->> (accounts/select-nested-by-entity-id (env :db) entity-id)
        (map #(opt-group-for-account-type % selected-id)))))

(defn- paced-progress-bar
  [data]
  [:span.paced-progress-bar {:data-max (:total-budget data)
                             :data-value (:actual data)
                             :data-pacer (:prorated-budget data)}])

(defn- budget-monitor-section
  [header data]
  (html
    [:h5 header]
    [:div.pull-right (-> data :total-budget format-number)]
    [:div (-> data :actual format-number)]
    [:span {:title (format "Target: %s, Actual: %s"
                           (-> data :prorated-budget format-number)
                           (-> data :actual format-number))}
     (paced-progress-bar data)]))

(defn- budget-monitor
  [monitor]
  [:div.panel.panel-default
   [:div.panel-heading
    [:form {:action (format "/entities/%s/monitors/%s/delete"
                            (-> monitor :account :entity-id)
                            (-> monitor :account :id))
            :method :post}
     [:button.close {:title "Click here to remove this budget monitor."}
      [:span {:aria-hidden true} "&times;"]]]
    [:strong (:caption monitor)]]
   [:div.panel-body
    (when (:message monitor)
      [:span.note (:message monitor)])
    (map #(when-let [data (% monitor)]
            (budget-monitor-section (humanize %) data))
         [:period :budget])]])

(defn budget-monitors
  [entity-id]
  (html
    [:h3 "Budget monitors"]
    (->> entity-id
         (entities/find-by-id (env :db))
         :monitored-account-ids
         (map (comp #(reports/monitor (env :db) %)
                    #(accounts/find-by-id (env :db) %)))
         (remove empty?)
         (map budget-monitor))
    [:a.btn.btn-primary {:href (format "/entities/%s/monitors" entity-id)}
     [:span.glyphicon.glyphicon-plus {:aria-hidden true}]]))