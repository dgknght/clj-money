(ns clj-money.import.gnucash
  (:refer-clojure :exclude [update])
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.data.xml :as xml]
            [clj-money.import :refer [read-source]])
  (:import java.util.zip.GZIPInputStream))

(defmulti process-node
  (fn [node]
    (if (string? node)
      :string
      (:tag node))))

(defmethod process-node :string
  [node]
  ; no action required
  )

(defmethod process-node :xmlns.http%3A%2F%2Fwww.gnucash.org%2FXML%2Fgnc/count-data
  [node]
  (pprint {:count-data node}))

(defmethod process-node :xmlns.http%3A%2F%2Fwww.gnucash.org%2FXML%2Fgnc/book
  [node]
  (doseq [node (:content node)]
    (process-node node)))

(defmethod process-node :xmlns.http%3A%2F%2Fwww.gnucash.org%2FXML%2Fbook/id
  [node]
  (pprint {:book/id node}))

(defmethod process-node :xmlns.http%3A%2F%2Fwww.gnucash.org%2FXML%2Fgnc/commodity
  [node]
  (pprint {:commodity node}))

(defmethod process-node :xmlns.http%3A%2F%2Fwww.gnucash.org%2FXML%2Fgnc/account
  [node]
  (println "account"))

(defmethod process-node :xmlns.http%3A%2F%2Fwww.gnucash.org%2FXML%2Fgnc/transaction
  [node]
  (println "transaction"))

(defmethod read-source :gnucash
  [input _]
  (let [root (-> (GZIPInputStream. input)
                 io/reader
                 xml/parse)]
    (doseq [node (:content root)]
      (process-node node))))
