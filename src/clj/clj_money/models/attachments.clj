(ns clj-money.models.attachments
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [config.core :refer [env]]
            [stowaway.core
             :refer [tag]]
            [stowaway.implicit
             :as storage
             :refer [with-storage
                     with-transacted-storage]]
            [dgknght.app-lib.models :refer [->id]]
            [dgknght.app-lib.validation :refer [with-validation]]
            [clj-money.models :as models]
            [clj-money.models.transactions :as transactions]
            [clj-money.models.images :as images])
  (:import org.joda.time.LocalDate))

(s/def ::id integer?)
(s/def ::transaction-id uuid?)
(s/def ::transaction-date #(instance? LocalDate %))
(s/def ::image-id integer?)
(s/def ::caption string?)
(s/def ::new-attachment (s/keys :req-un [::transaction-id
                                         ::transaction-date
                                         ::image-id]
                                :opt-un [::caption]))
(s/def ::existing-attachment (s/keys :req-un [::id]
                                     :opt-un [::caption]))

(defn- after-read
  [attachment]
  (when attachment
    (tag attachment ::models/attachment)))

(defn- before-save
  [attachment]
  (tag attachment ::models/attachment))

(defn- update-transaction
  [attachment f]
  (-> (transactions/find attachment)
      (update-in [:attachment-count] f)
      transactions/update))

(defn create
  [attachment]
  (with-transacted-storage (env :db)
    (with-validation attachment ::new-attachment
      (update-transaction attachment inc)
      (-> attachment
          before-save
          storage/create
          after-read))))

(defn search
  ([criteria]
   (search criteria {}))
  ([criteria options]
   (with-storage (env :db)
     (map after-read
          (storage/select (tag criteria ::models/attachment)
                          options)))))

(defn find-by
  ([criteria]
   (find-by criteria {}))
  ([criteria options]
   (first (search criteria (assoc options :limit 1)))))

(defn find
  [attachment-or-id]
  (find-by {:id (->id attachment-or-id)}))

(defn update
  [attachment]
  (with-storage (env :db)
    (with-validation attachment ::existing-attachment
      (-> attachment
          before-save
          storage/update)
      (find attachment))))

(defn delete
  [id-or-attachment]
  (with-transacted-storage (env :db)
    (let [attachment (if (integer? id-or-attachment)
                       (find id-or-attachment)
                       id-or-attachment)
          image (images/find (:image-id attachment))]
      (images/delete image)
      (storage/delete attachment)
      (update-transaction attachment dec))))
