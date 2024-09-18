(ns clj-money.api.accounts
  (:refer-clojure :exclude [update get])
  (:require [dgknght.app-lib.web :refer [unserialize-date
                                         unserialize-date-time]]
            [dgknght.app-lib.core :refer [parse-int
                                          update-in-if]]
            [dgknght.app-lib.decimal :as decimal :refer [->decimal]]
            [dgknght.app-lib.api-async :as api]
            [clj-money.api :refer [handle-ex]]
            [clj-money.state :refer [current-entity]]))

(defn- set-flags ; TODO: maybe the checkbox form fn should be able to handle keywords in a set?
  [{:keys [tags] :as account}]
  (reduce #(assoc %1 %2 (boolean (tags %2)))
          account
          tags))

(defn- correct-allocations
  [allocations]
  (->> allocations
       (map #(-> %
                 (update-in [0] (comp parse-int name))
                 (update-in [1] ->decimal)))
       (into {})))

(defn- after-read
  [account]
  (-> account
      (update-in [:type] keyword)
      (update-in [:system-tags] (comp set #(map keyword %)))
      (update-in [:user-tags] (comp set #(map keyword %)))
      (update-in [:value] ->decimal)
      (update-in [:quantity] ->decimal)
      (update-in [:tags] #(->> % (map keyword) set))
      (update-in-if [:allocations] correct-allocations)
      set-flags
      (update-in [:created-at] unserialize-date-time)
      (update-in [:commodity :type] keyword)
      (update-in [:earliest-transaction-date] unserialize-date)
      (update-in [:latest-transaction-date] unserialize-date)))

(defn- transform
  [xf]
  (comp (api/apply-fn after-read)
        xf))

(defn select
  ([xf]
   (select {} xf))
  ([criteria xf]
   (api/get (api/path :entities (:id @current-entity) :accounts)
            criteria
            {:transform (transform xf)
             :handle-ex (handle-ex "Unable to get the list of acounts: %s")})))

(defn get
  [id xf]
  (api/get (api/path :accounts id)
           {:transform (transform xf)
            :handle-ex "Unable to get the account: %s"}))

(def ^:private attribute-keys
  [:id
   :name
   :entity-id
   :type
   :commodity-id
   :parent-id
   :allocations
   :trading
   :system-tags
   :user-tags])

(defn create
  [account xf]
  (api/post (api/path :entities (:entity-id account) :accounts)
            (select-keys account attribute-keys)
            {:transform (transform xf)
             :handle-ex (handle-ex "Unable to create the account: %s")}))

(defn update
  ([account xf]
   (update account xf (handle-ex "Unable to update the account: %s")))
  ([account xf ex-handler]
   (api/patch (api/path :accounts (:id account))
              (select-keys account attribute-keys)
              {:transform (transform xf)
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
