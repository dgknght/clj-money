(ns clj-money.models.attachments
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec :as s]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.models.helpers :refer [with-storage
                                              with-transacted-storage]]
            [clj-money.models.storage :refer [create-attachment
                                                select-attachments]]))

(defn create
  [storage-spec attachment]
  (with-storage [s storage-spec]
    (create-attachment s attachment)))

(defn search
  [storage-spec criteria]
  (with-storage [s storage-spec]
    (select-attachments s criteria)))
