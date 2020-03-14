(ns clj-money.models.attachments
  (:refer-clojure :exclude [update])
  (:require
            [clojure.spec.alpha :as s]
            [clj-money.util :refer [rev-args]]
            [clj-money.models :as models]
            [clj-money.models.helpers :refer [with-storage
                                              with-transacted-storage
                                              create-fn]]
            [clj-money.models.storage :refer [create-attachment
                                              select-attachments
                                              delete-attachment]]
            [clj-money.models.images :as images])
  (:import org.joda.time.LocalDate))

(s/def ::transaction-id uuid?)
(s/def ::transaction-date #(instance? LocalDate %))
(s/def ::image-id integer?)
(s/def ::caption string?)
(s/def ::new-attachment (s/keys :req-un [::transaction-id
                                         ::transaction-date
                                         ::caption
                                         ::image-id]))

(defn- after-read
  [attachment & _]
  (when attachment
    (models/tag attachment ::models/attachment)))

(def create
  (create-fn {:create (rev-args create-attachment)
              :after-read after-read
              :spec ::new-attachment}))

(defn search
  ([storage-spec criteria]
   (search storage-spec criteria {}))
  ([storage-spec criteria options]
   (with-storage [s storage-spec]
     (map after-read
          (select-attachments s
                              criteria
                              options)))))

(defn find-by-id
  [storage-spec id]
  (->> (search storage-spec {:id id} {:limit 1})
       first
       after-read))

(defn delete
  [storage-spec id-or-attachment]
  (with-transacted-storage [s storage-spec]
    (let [attachment (if (integer? id-or-attachment)
                       (find-by-id s id-or-attachment)
                       id-or-attachment)]
      (images/delete s (:image-id attachment))
      (delete-attachment s (:id attachment)))))
