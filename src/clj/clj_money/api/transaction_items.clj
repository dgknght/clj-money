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
  (coercion/coerce criteria criteria-coercion-rules))

(coercion/register-coerce-fn :sort (fn [s]
                                     (when (and (s (seq s)))
                                       (map keyword s))))

(def ^:private options-coercion-rules
  [(coercion/rule :keyword [:sort 0])
   (coercion/rule :keyword [:sort 1])])

(defn- prepare-options
  [options]
  (coercion/coerce options options-coercion-rules))

(defn index
  [{{:keys [criteria options] :as params} :params :as req}]
  (->response (transactions/search-items (env :db)
                                         (prepare-criteria criteria)
                                         (prepare-options options))))
