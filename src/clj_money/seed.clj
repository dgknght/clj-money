(ns clj-money.seed
  (:require [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [clj-money.serialization :refer [realize]]
            [clj-money.models.helpers :refer [with-storage
                                              with-transacted-storage]]
            [clj-money.models.entities :as entities]
            [clj-money.models.users :as users]))

(defn- append-entity
  [storage-spec entity context]
  (update-in context
             [:entities]
             (fnil #(conj % (entities/find-by-name storage-spec
                                                 (-> context :users first)
                                                 entity))
                   [])))

(defn- append-user
  [storage-spec user context]
  (update-in context
             [:users]
             (fnil #(conj % (users/find-by-email storage-spec user)) [])))

(defn seed
  [user entity identifier]
  (with-transacted-storage [s (env :db)]
    (->> identifier
         (format "resources/seeds/%s.edn")
         slurp
         read-string
         (append-user s user)
         (append-entity s entity)
         (realize s))))
