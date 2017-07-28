(ns clj-money.models.attachments
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec :as s]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.models.helpers :refer [with-storage
                                              with-transacted-storage
                                              create-fn
                                              update-fn]]
            [clj-money.models.storage :refer [create-attachment
                                              select-attachments
                                              delete-attachment]]
            [clj-money.models.images :as images]))

(s/def ::transaction-id integer?)
(s/def ::image-id integer?)
(s/def ::caption string?)
(s/def ::new-attachment (s/keys :req-un [::transaction-id
                                         ::caption
                                         ::image-id]))

(def create
  (create-fn {:create create-attachment
              :spec ::new-attachment}))

(defn search
  ([storage-spec criteria]
   (search storage-spec criteria {}))
  ([storage-spec criteria options]
   (with-storage [s storage-spec]
     (select-attachments s criteria))))

(defn find-by-id
  [storage-spec id]
  (first (search storage-spec {:id id} {:limit 1})))

(defn delete
  [storage-spec id-or-attachment]
  (with-transacted-storage [s storage-spec]
    (let [attachment (if (integer? id-or-attachment)
                       (find-by-id s id-or-attachment)
                       id-or-attachment)]
      (images/delete s (:image-id attachment))
      (delete-attachment s (:id attachment)))))
