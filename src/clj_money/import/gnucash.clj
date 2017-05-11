(ns clj-money.import.gnucash
  (:refer-clojure :exclude [update])
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [clj-xpath.core :refer :all]
            [clj-money.util :refer [pprint-and-return]]
            [clj-money.import :refer [read-source]])
  (:import java.util.zip.GZIPInputStream))

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

(def ^:private namespace-map
  {"gnc" "http://www.gnucash.org/XML/gnc"
   "act" "http://www.gnucash.org/XML/act"})

(defmethod read-source :gnucash
  [_ input callback]
  (with-namespace-context namespace-map
    (let [xml (->> (GZIPInputStream. input)
                   io/reader
                   slurp
                   xml->doc)]
      (doseq [node ($x "/gnc-v2/gnc:book/gnc:account" xml)]
        (process-node callback node)))))
