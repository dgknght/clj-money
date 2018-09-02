(ns clj-money.api.transaction-items
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [clojure.spec.alpha :as s]
            [clojure.set :refer [rename-keys]]
            [environ.core :refer [env]]
            [ring.util.response :refer [status response header]]
            [clj-money.api :refer [->response
                                   invalid->response
                                   error->response
                                   index-resource
                                   create-resource
                                   update-resource
                                   delete-resource]]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.authorization :refer [authorize
                                             tag-resource]]
            [clj-money.models.transactions :as transactions]
            [clj-money.permissions.transactions]))

(defn index
  [{params :params}]

  (log/debug "dbk api.transaction-items/index " (prn-str params))

  (index-resource transactions/search-items
                  (-> params
                      (rename-keys {:id :account-id})
                      (select-keys [:account-id :transaction-date]))
                  :transaction-item))
