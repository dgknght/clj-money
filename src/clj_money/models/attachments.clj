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
^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(s/def ::models/attachment (s/keys :req [:attachment/transaction
                                         :attachment/transaction-date
                                         :attachment/image]
                                   :opt [:attachment/caption]))

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
  [_before att]
  [att
   (adjust-trx att inc)])

(defmethod models/propagate-delete :attachment
  [att]
  [att
   (adjust-trx att dec)])
