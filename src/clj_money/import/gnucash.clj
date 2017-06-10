(ns clj-money.import.gnucash
  (:refer-clojure :exclude [update])
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [clj-time.core :as t]
            [clj-xpath.core :refer :all]
            [clj-money.util :refer [pprint-and-return]]
            [clj-money.import :refer [read-source]])
  (:import java.util.zip.GZIPInputStream))

(defn- parse-date
  [string-date]
  (when-let [match (re-find #"^(\d{4})-(\d{2})-(\d{2})" string-date)]
    (apply t/local-date (->> match
                             rest
                             (map #(Integer. %))))))

(defn- parse-decimal
  [string-decimal]
  (when-let [match (re-find #"(\d+)\/(\d+)" string-decimal)]
    (apply / (->> match
                  rest
                  (map bigdec)))))

(def ^:private namespace-map
  {"gnc"        "http://www.gnucash.org/XML/gnc"
   "act"        "http://www.gnucash.org/XML/act"
   "trn"        "http://www.gnucash.org/XML/trn"
   "ts"         "http://www.gnucash.org/XML/ts"
   "split"      "http://www.gnucash.org/XML/split"
   "bgt"        "http://www.gnucash.org/XML/bgt"
   "recurrence" "http://www.gnucash.org/XML/recurrence"
   "slot"       "http://www.gnucash.org/XML/slot"
   "cd"         "http://www.gnucash.org/XML/cd"})

(defmulti process-node
  (fn [_ node]
    (:tag node)))

(defn- process-node-attribute
  [node result {:keys [attribute xpath transform-fn]}]
  (let [transform-fn (if transform-fn
                       transform-fn
                       identity)
        raw-value ($x:text? xpath node)
        value (transform-fn raw-value)]
    (assoc result attribute value)))

(defn- node->model
  [node attributes]
  (reduce (partial process-node-attribute node) {} attributes))

(def ^:private account-types-map
  {"ASSET"      :asset
   "BANK"       :asset
   "INCOME"     :income
   "EXPENSE"    :expense
   "LIABILITY"  :liability
   "CREDIT"     :liability})

(def ^:private account-attributes
  [{:attribute :name
    :xpath "act:name"}
   {:attribute :type
    :xpath "act:type"
    :transform-fn account-types-map}
   {:attribute :id
    :xpath "act:id"}
   {:attribute :parent-id
    :xpath "act:parent"}])

(def ^:private ignored-accounts #{"Assets" "Liabilities" "Equity" "Income" "Expenses"})

(defn- include-account?
  [account]
  (and (:type account)
       (not (ignored-accounts (:name account)))))

(defmethod process-node :gnc:account
  [callback node]
  (let [account (node->model node account-attributes)]
    (when (include-account? account)
      ((.account callback) account))))

(def ^:private budget-attributes
  [{:attribute :id
    :xpath "bgt:id"}
   {:attribute :name
    :xpath "bgt:name"}
   {:attribute :start-date
    :xpath "bgt:recurrence/recurrence:start/gdate"
    :transform-fn parse-date}
   {:attribute :period
    :xpath "bgt:recurrence/recurrence:period_type"
    :transform-fn keyword}
   {:attribute :period-count
    :xpath "bgt:num-periods"
    :transform-fn #(Integer. %)}])

(def ^:private budget-item-attributes
  [{:attribute :account-id
    :xpath "slot:key"}])

(def ^:private budget-item-period-attributes
  [{:attribute :index
    :xpath "slot:key"
    :transform-fn #(Integer. %)}
   {:attribute :amount
    :xpath "slot:value"
    :transform-fn parse-decimal}])

(defn- node->budget-item-period
  [node]
  (with-namespace-context namespace-map
    (node->model node budget-item-period-attributes)))

(defn- node->budget-item
  [node]
  (with-namespace-context namespace-map
    (-> node
        (node->model budget-item-attributes)
        (assoc :periods (->> node
                             ($x "slot:value/slot")
                             (map node->budget-item-period)
                             (into #{}))))))

(defmethod process-node :gnc:budget
  [callback node]
  (-> node
      (node->model budget-attributes)
      (assoc :items (map node->budget-item ($x "bgt:slots/slot" node)))
      ((.budget callback))))

(defmethod process-node :gnc:count-data
  [callback node]
  (let [declaration {:record-type (keyword (-> node :attrs :cd:type))
                     :record-count (Integer. (:text node))}]
    ((.declaration callback) declaration)))

(def ^:private transaction-item-attributes
  [{:attribute :account-id
    :xpath "split:account"}
   {:attribute :reconciled
    :xpath "split:reconciled-state"
    :transform-fn #(= "y" %)}
   {:attribute :amount
    :xpath "split:value"
    :transform-fn parse-decimal}
   {:attribute :action
    :xpath "split:value"
    :transform-fn #(if (= \- (first %))
                     :credit
                     :debit)}])

(defn- node->transaction-item
  [node]
  ; I'm not sure why I need to add the namespace
  ; context again here
  (with-namespace-context namespace-map
    (node->model node transaction-item-attributes)))

(def ^:private transaction-attributes
  [{:attribute :id
    :xpath "trn:id"}
   {:attribute :transaction-date
    :xpath "trn:date-posted/ts:date"
    :transform-fn parse-date}
   {:attribute :description
    :xpath "trn:description"}])

(defn- node->transaction
  [node]
  (-> node
      (node->model transaction-attributes)
      (assoc :items (->> ($x "trn:splits/trn:split" node)
                         (map node->transaction-item)))))

(defmethod process-node :gnc:transaction
  [callback node]
  ((.transaction callback) (node->transaction node)))

(defmethod read-source :gnucash
  [_ input callback]
  (with-namespace-context namespace-map
    (let [xml (->> (GZIPInputStream. input)
                   io/reader
                   slurp
                   xml->doc)]
      (doseq [node ($x "/gnc-v2/gnc:book/gnc:count-data | /gnc-v2/gnc:book/gnc:account | /gnc-v2/gnc:book/gnc:transaction | /gnc-v2/gnc:book/gnc:budget" xml)]
        (process-node callback node)))))
