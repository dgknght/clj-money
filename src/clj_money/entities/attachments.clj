(ns clj-money.entities.attachments
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [dgknght.app-lib.core :refer [fmin]]
            [clj-money.util :as util]
            [clj-money.entities :as entities]
            [clj-money.entities.propagation :as prop]))

(s/def :attachment/transaction ::entities/entity-ref)
(s/def :attachment/image ::entities/entity-ref)
(s/def :attachment/caption string?)
(s/def ::entities/attachment (s/keys :req [:attachment/transaction
                                         :attachment/image]
                                   :opt [:attachment/caption]))

(defmethod entities/before-validation :attachment
  [{:as att :attachment/keys [transaction]}]
  (if (and (:transaction/transaction-date transaction)
           (nil? (:attachment/transaction-date att)))
    (assoc att :attachment/transaction-date (:transaction/transaction-date transaction))
    att))

(defn- find-trx
  [{:attachment/keys [transaction transaction-date]}]
  (entities/find-by
    (cond-> (util/entity-type {:id (:id transaction)}
                             :transaction)
      transaction-date (assoc :transaction/transaction-date
                              transaction-date))))

(defn- adjust-trx
  [att f]
  (-> (find-trx att)
      (dissoc :transaction/items)
      (update-in [:transaction/attachment-count]
                 (fnil f 0))))

(defmethod prop/propagate :attachment
  [[before after]]
  (log/infof "[propagation] %s -> %s" before after)
  (cond-> []
    (and after (not before))
    (conj (adjust-trx after inc))

    (and before (not after))
    (conj (adjust-trx before (fmin dec 0)))))

(defn propagate-all
  "Update attachment counts for transactions that have attachments. NB: This
  does not reset the count for transactions without attachments to zero."
  [entity _opts]
  (log/infof "[propagation] start entity %s"
             (:entity/name entity))
  (when-let [attachments (seq
                           (entities/select
                             (util/entity-type
                               {:transaction/entity entity}
                               :attachment)))]
    (let [updates (->> attachments
                       (group-by (comp :id :attachment/transaction))
                       (map #(update-in % [1] count))
                       (into {}))]
      (->> (entities/find-many (keys updates))
           (map (comp
                  #(dissoc % :transaction/items)
                  #(assoc % :transaction/attachment-count (updates (:id %)))))
           entities/put-many))))

(prop/add-full-propagation propagate-all)
