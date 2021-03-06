(ns clj-money.api.accounts
  (:refer-clojure :exclude [update get])
  (:require [dgknght.app-lib.web :refer [unserialize-date
                                         unserialize-date-time]]
            [clj-money.state :refer [current-entity]]
            [dgknght.app-lib.decimal :as decimal :refer [->decimal]]
            [dgknght.app-lib.api :as api]))

(defn- set-flags ; TODO: maybe the checkbox form fn should be able to handle keywords in a set?
  [{:keys [tags] :as account}]
  (reduce #(assoc %1 %2 (boolean (tags %2)))
          account
          tags))

(defn- after-read
  [account]
  (-> account
      (update-in [:type] keyword)
      (update-in [:system-tags] (comp set #(map keyword %)))
      (update-in [:user-tags] (comp set #(map keyword %)))
      (update-in [:value] ->decimal)
      (update-in [:quantity] ->decimal)
      (update-in [:tags] #(->> % (map keyword) set))
      set-flags
      (update-in [:created-at] unserialize-date-time)
      (update-in [:commodity :type] keyword)
      (update-in [:earliest-transaction-date] unserialize-date)
      (update-in [:latest-transaction-date] unserialize-date)))

(defn select
  ([success-fn error-fn]
   (select {} success-fn error-fn))
  ([criteria success-fn error-fn]
   (api/get (api/path :entities (:id @current-entity) :accounts)
            criteria
            #(success-fn (map after-read %))
            error-fn)))

(defn get
  [id success-fn error-fn]
  (api/get (api/path :accounts id)
           #(success-fn (after-read %))
           error-fn))

(def ^:private attribute-keys
  [:id
   :name
   :entity-id
   :type
   :commodity-id
   :parent-id
   :trading
   :system-tags
   :user-tags])

(defn create
  [account success-fn error-fn]
  (api/post (api/path :entities (:entity-id account) :accounts)
            (select-keys account attribute-keys)
            success-fn
            error-fn))

(defn update
  [account success-fn error-fn]
  (api/patch (api/path :accounts (:id account))
             (select-keys account attribute-keys)
             success-fn
             error-fn))

(defn save
  [account success-fn error-fn]
  (if (:id account)
    (update account success-fn error-fn)
    (create account success-fn error-fn)))

(defn delete
  [account success-fn error-fn]
  (api/delete (api/path :accounts (:id account))
              success-fn
              error-fn))
