(ns clj-money.models.attachments
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [fmin
                                          update-in-if]]
            [clj-money.util :as util]
            [clj-money.models :as models]
            [clj-money.models.propagation :as prop]))

(s/def :attachment/transaction ::models/model-ref)
(s/def :attachment/transaction-date t/local-date?)
(s/def :attachment/image ::models/model-ref)
(s/def :attachment/caption string?)
^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(s/def ::models/attachment (s/keys :req [:attachment/transaction
                                         :attachment/image]
                                   :opt [:attachment/caption
                                         :attachment/transaction-date]))

(defmethod models/before-validation :attachment
  [{:as att :attachment/keys [transaction]}]
  (if (and (:transaction/transaction-date transaction)
           (nil? (:attachment/transaction-date att)))
    (assoc att :attachment/transaction-date (:transaction/transaction-date transaction))
    att))

(defn- find-trx
  [{:attachment/keys [transaction transaction-date]}]
  (models/find-by
    (cond-> (util/model-type {:id (:id transaction)}
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
  ([opts]
   (doseq [e (models/select (util/model-type {} :entity))]
     (propagate-all e opts)))
  ([entity _opts]
   (some->> (models/select
              (util/model-type {:transaction/entity entity}
                               :attachment))
            seq
            (map #(adjust-trx % inc))
            (models/put-many))))

(prop/add-full-propagation propagate-all)
