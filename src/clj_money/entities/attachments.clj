(ns clj-money.entities.attachments
  (:refer-clojure :exclude [update find])
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
^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
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
  (update-in (find-trx att)
             [:transaction/attachment-count]
             (fnil f 0)))

(defmethod prop/propagate :attachment
  [[before after]]
  (cond-> []
    (and after (not before))
    (conj (adjust-trx after inc))

    (and before (not after))
    (conj (adjust-trx before (fmin dec 0)))))

(defn propagate-all
  [entity _opts]
  (log/debugf "[propagation] start entity %s"
              (:entity/name entity))
  (let [updated (some->> (entities/select
                           (util/entity-type {:transaction/entity entity}
                                            :attachment))
                         seq
                         (map #(adjust-trx % inc))
                         (entities/put-many))]
    (log/infof "[propagation] finish entity %s"
               (:entity/name entity))
    updated))

(prop/add-full-propagation propagate-all)
