(ns clj-money.api.accounts
  (:refer-clojure :exclude [update get])
  (:require [cljs.pprint :refer [pprint]]
            [clj-money.api :as api :refer [add-error-handler]]
            [clj-money.state :refer [current-entity]]))

(defn select
  [criteria & {:as opts}]
  (api/get (api/path :entities (:id @current-entity) :accounts)
           criteria
           (add-error-handler
             opts
             "Unable to get the list of accounts: %s")))

(defn create
  [account {:as opts}]
  (api/post (api/path :entities (:account/entity account) :accounts)
            account
            (add-error-handler
              opts
              "Unable to create the account: %s")))

(defn update
  [account {:as opts}]
  (api/patch (api/path :accounts (:id account))
             account
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
  (api/delete (api/path :accounts account)
              (add-error-handler
                opts
                "Unable to delete the account: %s")))
