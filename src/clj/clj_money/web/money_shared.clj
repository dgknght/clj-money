(ns clj-money.web.money-shared
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [clj-time.core :as t]
            [clj-time.format :refer [unparse-local-date
                                     formatters]]
            [environ.core :refer [env]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [ring.util.response :refer :all]
            [clj-money.util :refer [format-number
                                    descending-periodic-seq]]
            [clj-money.inflection :refer [humanize]]
            [clj-money.authorization :refer [authorize
                                             allowed?
                                             apply-scope]]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.transactions :as transactions]
            [clj-money.permissions.entities]
            [clj-money.permissions.accounts]
            [clj-money.reports :as reports])
  (:use [clj-money.web.shared :refer :all]))

(defn- account-and-children-options
  [account selected-id]
  (concat [[:option
            (cond-> {:value (:id account)}
              (= selected-id (:id account))
              (assoc :selected "selected"))
            (:path account)]]
          (map #(account-and-children-options % selected-id) (:children account))))

(defn- opt-group-for-account-type
  [{account-type :type accounts :accounts} selected-id]
  [:optgroup {:label (humanize account-type)}
   (mapcat #(account-and-children-options % selected-id)
        accounts)])

(defn grouped-options-for-accounts
  ([entity-id]
   (grouped-options-for-accounts entity-id {}))
  ([entity-id options]
   (let [optgroups (->> (apply-scope {:entity-id entity-id} :account)
                        (accounts/search (env :db))
                        accounts/nest
                        (filter #(or (nil? (:types options))
                                     ((:types options) (:type %))))
                        (map #(opt-group-for-account-type % (:selected-id options))))]
     (if (:include-none? options)
       (concat [[:option {:value ""} ""]] optgroups)
       optgroups))))

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
  [monitor entity]
  [:div.panel.panel-default
   [:div.panel-heading
    (form (format "/entities/%s/monitors/%s/delete"
                  (-> monitor :account :entity-id)
                  (-> monitor :account :id)) {}
          (when (allowed? :update entity)
            [:button.close {:title "Click here to remove this budget monitor."}
             [:span {:aria-hidden true} "&times;"]]))
    [:strong (:caption monitor)]]
   [:div.panel-body
    (when (:message monitor)
      [:span.note (:message monitor)])
    (map #(when-let [data (% monitor)]
            (budget-monitor-section (humanize %) data))
         [:period :budget])]])

(defn budget-monitors
  [entity-or-id]
  (let [entity (if (map? entity-or-id)
                 entity-or-id
                 (entities/find-by-id (env :db) entity-or-id))]
    (when (and entity (allowed? :show entity))
      (html
        [:h3 "Budget monitors"]
        (->> (-> entity
                 :settings
                 :monitored-account-ids)
             (map (comp #(reports/monitor (env :db) %)
                        #(accounts/find-by-id (env :db) %)))
             (remove empty?)
             (map #(budget-monitor % entity)))
        (when (allowed? :update entity)
          [:a.btn.btn-primary {:href (format "/entities/%s/monitors" (:id entity))}
           [:span.glyphicon.glyphicon-plus {:aria-hidden true}]])))))

(defn inventory-method-options
  []
  (map #(vector :option {:value (first %)} (second %))
       {:fifo "First in, first out"
        :lifo "Last in, first out"}))

(defn available-month-options
  [selected]
  (let [[start end] (transactions/available-date-range (env :db))]
    (map (fn [date]
           (let [formatted (unparse-local-date (:year-month formatters) date)]
             [:option {:value formatted
                       :selected (= formatted selected)}
              formatted]))
         (descending-periodic-seq start end (t/months 1)))))
