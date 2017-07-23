(ns clj-money.models.attachments
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec :as s]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.models.helpers :refer [with-storage
                                              create-fn
                                              update-fn]]
            [clj-money.models.storage :refer [create-attachment
                                                select-attachments]]))

(s/def ::transaction-id integer?)
(s/def ::image-id integer?)
(s/def ::caption string?)
(s/def ::mime-type string?)
(s/def ::new-attachment (s/keys :req-un [::transaction-id
                                         ::caption
                                         ::image-id
                                         ::mime-type]))

(def create
  (create-fn {:create create-attachment
              :spec ::new-attachment}))

(defn search
  [storage-spec criteria]
  (with-storage [s storage-spec]
    (select-attachments s criteria)))
