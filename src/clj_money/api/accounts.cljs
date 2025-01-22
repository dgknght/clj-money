(ns clj-money.api.accounts
  (:refer-clojure :exclude [update get])
  (:require [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.util :as util]
            [clj-money.api :as api :refer [handle-ex]]
            [clj-money.state :refer [current-entity]]))

(defn select
  ([xf]
   (select {} xf))
  ([criteria xf]
   (api/get (api/path :entities (:id @current-entity) :accounts)
            criteria
            {:transform xf
             :handle-ex (handle-ex "Unable to get the list of acounts: %s")})))

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
  [account xf]
  (api/post (api/path :entities (:account/entity account) :accounts)
            (-> account
                (update-in-if [:account/parent] util/->model-ref)
                (update-in [:account/commodity] util/->model-ref)
                (update-in [:account/entity] util/->model-ref)
                (select-keys attribute-keys)
                (util/pp-> ::ready))
            {:transform xf
             :handle-ex (handle-ex "Unable to create the account: %s")}))

(defn update
  ([account xf]
   (update account xf (handle-ex "Unable to update the account: %s")))
  ([account xf ex-handler]
   (api/patch (api/path :accounts (:id account))
              (select-keys account attribute-keys)
              {:transform xf
               :handle-ex ex-handler})))

(defn save
  [account xf]
  (if (:id account)
    (update account xf)
    (create account xf)))

(defn delete
  [account xf]
  (api/delete (api/path :accounts (:id account))
              {:transform xf
               :handle-ex (handle-ex "Unable to delete the account: %s")}))
