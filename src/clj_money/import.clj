(ns clj-money.import
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clj-money.models.helpers :refer [with-transacted-storage]]))

(defmulti read-source
  (fn [source-type _ _]
    source-type))

(deftype Callback [account transaction])

(defn- import-account
  [storage account]
  (pprint {:account account}))

(defn import-data
  "Reads the contents from the specified input and saves
  the information using the specified storage. If an entity
  with the specified name is found, it is used, otherwise it
  is created"
  [storage-spec entity-name input source-type]
  (with-transacted-storage [s storage-spec]
    (read-source source-type input (->Callback (partial import-account s)))))
