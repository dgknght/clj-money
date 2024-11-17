(ns clj-money.models.attachments
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [clj-money.models :as models]))

(s/def :attachment/transaction ::models/model-ref)
(s/def :attachment/transaction-date t/local-date?)
(s/def :attachment/image ::models/model-ref)
(s/def :attachment/caption string?)
(s/def ::models/attachment (s/keys :req [:attachment/transaction
                                         :attachment/transaction-date
                                         :attachment/image]
                                   :opt [:attachment/caption]))

(defn ^:deprecated create
  [_attachment]
  (throw (UnsupportedOperationException. "create is deprecated"))
  #_(with-transacted-storage (env :db)
    (with-validation attachment ::new-attachment
      (update-transaction attachment inc)
      (-> attachment
          before-save
          storage/create
          after-read))))

(defn ^:deprecated search
  ([criteria]
   (search criteria {}))
  ([_criteria _options]
   (throw (UnsupportedOperationException. "search is deprecated"))
   #_(with-storage (env :db)
     (map after-read
          (storage/select (tag criteria ::models/attachment)
                          options)))))

(defn ^:deprecated find-by
  ([criteria]
   (find-by criteria {}))
  ([_criteria _options]
   (throw (UnsupportedOperationException. "search is deprecated"))
   #_(first (search criteria (assoc options :limit 1)))))

(defn ^:deprecated find
  [_attachment-or-id]
   (throw (UnsupportedOperationException. "find is deprecated"))
  #_(find-by {:id (->id attachment-or-id)}))

(defn ^:deprecated update
  [_attachment]
   (throw (UnsupportedOperationException. "update is deprecated"))
  #_(with-storage (env :db)
    (with-validation attachment ::existing-attachment
      (-> attachment
          before-save
          storage/update)
      (find attachment))))

(defn ^:deprecated delete
  [_id-or-attachment]
   (throw (UnsupportedOperationException. "delete is deprecated"))
  #_(with-transacted-storage (env :db)
    (let [attachment (if (integer? id-or-attachment)
                       (find id-or-attachment)
                       id-or-attachment)
          image (images/find (:image-id attachment))]
      (images/delete image)
      (storage/delete attachment)
      (update-transaction attachment dec))))

(defmethod models/before-validation :attachment
  [{:as att :attachment/keys [transaction]}]
  (update-in att [:attachment/transaction-date] (fnil identity (:transaction/transaction-date transaction))))

(defn- find-trx
  [att]
  (models/find-by {:id (get-in att [:attachment/transaction :id])
                   :transaction/transaction-date (:attachment/transaction-date att)}))

(defn- adjust-trx
  [att f]
  (update-in (find-trx att)
             [:transaction/attachment-count]
             (fnil f 0)))

(defmethod models/propagate :attachment
  [att]
  [att
   (adjust-trx att inc)])

(defmethod models/propagate-delete :attachment
  [att]
  [att
   (adjust-trx att dec)])
