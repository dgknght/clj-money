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

(def ^:private namespace-map
  {"gnc"   "http://www.gnucash.org/XML/gnc"
   "act"   "http://www.gnucash.org/XML/act"
   "trn"   "http://www.gnucash.org/XML/trn"
   "ts"    "http://www.gnucash.org/XML/ts"
   "split" "http://www.gnucash.org/XML/split"})

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

(defmethod process-node :gnc:account
  [callback node]
  (let [account (node->model node account-attributes)]
    (when (:type account)
      ((.account callback) account))))

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
      (doseq [node ($x "/gnc-v2/gnc:book/gnc:account | /gnc-v2/gnc:book/gnc:transaction" xml)]
        (process-node callback node)))))
