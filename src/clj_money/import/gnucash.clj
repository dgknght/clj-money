(ns clj-money.import.gnucash
  (:refer-clojure :exclude [update])
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [clojure.data.xml :as xml]
            [clj-money.util :refer [pprint-and-return]]
            [clj-money.import :refer [read-source]])
  (:import java.util.zip.GZIPInputStream))

(defmulti process-node
  (fn [_ node]
    (if (string? node)
      :string
      (:tag node))))

(defmethod process-node :string
  [callback node]
  ; no action required
  )

(defmethod process-node :xmlns.http%3A%2F%2Fwww.gnucash.org%2FXML%2Fgnc/count-data
  [callback node]
  #_(pprint {:count-data node}))

(defmethod process-node :xmlns.http%3A%2F%2Fwww.gnucash.org%2FXML%2Fgnc/book
  [callback node]
  ; TODO Maybe this is an entity?
  (doseq [node (:content node)]
    (process-node callback node)))

(defmethod process-node :xmlns.http%3A%2F%2Fwww.gnucash.org%2FXML%2Fbook/id
  [callback node]
  ; can we ignore this?
  )

(defn- node->map
  [node]
  (->> node
       :content
       (remove string?)
       (reduce #(assoc %1 (:tag %2) (-> %2 :content first)) {})))

(defmethod process-node :xmlns.http%3A%2F%2Fwww.gnucash.org%2FXML%2Fgnc/commodity
  [callback node]
  ; TODO come back and get this later
  #_(pprint {:commodity node}))

(def ^:private account-attributes-map
  {:xmlns.http%3A%2F%2Fwww.gnucash.org%2FXML%2Fact/id :id
   :xmlns.http%3A%2F%2Fwww.gnucash.org%2FXML%2Fact/name :name
   :xmlns.http%3A%2F%2Fwww.gnucash.org%2FXML%2Fact/type :type
   :xmlns.http%3A%2F%2Fwww.gnucash.org%2FXML%2Fact/parent :parent-id})

(def ^:private account-types-map
  {"ASSET"      :asset
   "BANK"       :asset
   "INCOME"     :income
   "EXPENSE"    :expense
   "LIABILITY"  :liability
   "CREDIT"     :liability})

(defn- node->account
  [node]
  (-> node
      node->map
      (rename-keys account-attributes-map)
      (select-keys (vals account-attributes-map))
      (update-in [:type] account-types-map)))

(defmethod process-node :xmlns.http%3A%2F%2Fwww.gnucash.org%2FXML%2Fgnc/account
  [callback node]
  (let [account (node->account node)]
    (when (:type account)
      ((.account callback) account))))

(defmethod process-node :xmlns.http%3A%2F%2Fwww.gnucash.org%2FXML%2Fgnc/transaction
  [callback node]
  #_(println "transaction"))

(defmethod read-source :gnucash
  [_ input callback]
  (let [root (-> (GZIPInputStream. input)
                 io/reader
                 xml/parse)]
    (doseq [node (:content root)]
      (process-node callback node))))
