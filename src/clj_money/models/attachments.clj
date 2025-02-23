(ns clj-money.models.attachments
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [dgknght.app-lib.core :refer [fmin]]
            [clj-money.util :as util]
            [clj-money.models :as models]))

(s/def :attachment/transaction ::models/model-ref)
(s/def :attachment/transaction-date t/local-date?)
(s/def :attachment/image ::models/model-ref)
(s/def :attachment/caption string?)
^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(s/def ::models/attachment (s/keys :req [:attachment/transaction
                                         :attachment/transaction-date
                                         :attachment/image]
                                   :opt [:attachment/caption]))

(defmethod models/before-validation :attachment
  [{:as att :attachment/keys [transaction]}]
  (update-in att [:attachment/transaction-date] (fnil identity (:transaction/transaction-date transaction))))

(defn- find-trx
  [{:attachment/keys [transaction transaction-date]}]
  (models/find-by {:id (:id transaction)
                   :transaction/transaction-date transaction-date}))

(defn- adjust-trx
  [att f]
  (update-in (find-trx att)
             [:transaction/attachment-count]
             (fnil f 0)))

(defmethod models/propagate :attachment
  [[before after]]
  (cond-> []
    (and after (not before))
    (conj (adjust-trx after inc))

    (and before (not after))
    (conj (adjust-trx before (fmin dec 0)))))

(defn propagate-all
  ([]
   (doseq [e (models/select (util/model-type {} :entity))]
     (propagate-all e)))
  ([entity]
   (->> (models/select
          (util/model-type {:transaction/entity entity}
                           :attachment))
        (map #(adjust-trx % inc))
        (models/put-many))))

(models/add-full-propagation propagate-all)
