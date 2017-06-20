(ns clj-money.import.gnucash
  (:refer-clojure :exclude [update])
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [clojure.string :as s]
            [clojure.tools.logging :as log]
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
  (->> ["gnc" "act" "trn" "ts" "split" "bgt" "recurrence" "slot" "cd" "cmdty" "price"]
       (map #(vector % (format "http://www.gnucash.org/XML/%s" %)))
       (into {})))

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
    :transform-fn #(get account-types-map % :equity)}
   {:attribute :id
    :xpath "act:id"}
   {:attribute :parent-id
    :xpath "act:parent"}])

(def ^:private ignored-accounts #{"Root Account" "Assets" "Liabilities" "Equity" "Income" "Expenses"})

(defn- include-account?
  [account]
  (not (ignored-accounts (:name account))))

(defmethod process-node :gnc:account
  [callback node]
  (let [account (node->model node account-attributes)]
    (if (include-account? account)
      (callback account :account)
      ; when ignoring an account, make the callback
      ; so that the progress is updated (total count
      ; of imported accounts should match the declared
      ; count), but pass nil so that nothing is
      ; imported
      (callback nil :account))))

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
      (callback :budget)))

(def ^:private commodity-attributes
  [{:attribute :exchange
    :xpath "cmdty:space"
    :transform-fn (comp keyword s/lower-case)}
   {:attribute :symbol
    :xpath "cmdty:id"}
   {:attribute :name
    :xpath "cmdty:name"}])

(defmethod process-node :gnc:commodity
  [callback node]
  (let [commodity (node->model node commodity-attributes)]
    (when (not= :template (:exchange commodity))
      (callback (when (#{:nasdaq} (:exchange commodity))
                  commodity)
                :commodity))))

(def ^:private price-attributes
  [{:attribute :trade-date
    :xpath "price:time/ts:date"
    :transform-fn parse-date}
   {:attribute :price
    :xpath "price:value"
    :transform-fn parse-decimal}
   {:attribute :exchange
    :xpath "price:commodity/cmdty:space"
    :transform-fn (comp keyword s/lower-case)}
   {:attribute :symbol
    :xpath "price:commodity/cmdty:id"}])

(defmethod process-node :price
  [callback node]
  (-> node
      (node->model price-attributes)
      (callback :price)))

(defmethod process-node :gnc:count-data
  [callback node]
  (let [declaration {:record-type (keyword (-> node :attrs :cd:type))
                     :record-count (Integer. (:text node))}]
    (callback declaration :declaration)))

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
  (callback (node->transaction node) :transaction))

(def element-xpath
  (->> ["count-data" "account" "transaction" "budget" "commodity"]
       (map #(format "/gnc-v2/gnc:book/gnc:%s" %))
       (concat ["/gnc-v2/gnc:book/gnc:pricedb/price"])
       (s/join " | ")))

(defmethod read-source :gnucash
  [_ input callback]
  (with-namespace-context namespace-map
    (let [xml (->> (GZIPInputStream. input)
                   io/reader
                   slurp
                   xml->doc)]
      (doseq [node ($x element-xpath xml)]
        (process-node callback node)))))
