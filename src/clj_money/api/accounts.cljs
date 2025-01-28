(ns clj-money.api.accounts
  (:refer-clojure :exclude [update get])
  (:require [cljs.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.util :as util]
            [clj-money.api :as api :refer [add-error-handler]]
            [clj-money.state :refer [current-entity]]))

(defn select
  [criteria & {:as opts}]
  (api/get (api/path :entities (:id @current-entity) :accounts)
           criteria
           (add-error-handler
             opts
             "Unable to get the list of acounts: %s")))

(def ^:private attribute-keys
  [:id
   :account/name
   :account/entity
   :account/type
   :account/commodity
   :account/parent
   :account/allocations
   :account/trading
   :account/system-tags
   :account/user-tags])

(defn create
  [account {:as opts}]
  (api/post (api/path :entities (:account/entity account) :accounts)
            (-> account
                (update-in-if [:account/parent] util/->model-ref)
                (update-in [:account/commodity] util/->model-ref)
                (update-in [:account/entity] util/->model-ref)
                (select-keys attribute-keys)
                (util/pp-> ::ready))
            (add-error-handler
              opts
              "Unable to create the account: %s")))

(defn update
  [account {:as opts}]
  (api/patch (api/path :accounts (:id account))
             (select-keys account attribute-keys)
             (add-error-handler
               opts
               "Unable to update the account: %s")))

(defn save
  [account & {:as opts}]
  (if (:id account)
    (update account opts)
    (create account opts)))

(defn delete
  [account & {:as opts}]
  (api/delete (api/path :accounts (:id account))
              (add-error-handler
                opts
                "Unable to delete the account: %s")))
