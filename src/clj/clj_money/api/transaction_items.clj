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

(def ^:private criteria-coercion-rules
  [(coercion/rule :integer [:account-id])])

(defn- prepare-criteria
  [criteria]
  #_(-> criteria
      (update-in [:])
      )
  criteria)

(defn- prepare-options
  [options]
  options)

(defn index
  [{{:keys [criteria options] :as params} :params}]
  (->response (transactions/search-items (env :db)
                                         (prepare-criteria criteria)
                                         (prepare-options options))))
