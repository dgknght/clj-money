(ns clj-money.models.imports
  (:refer-clojure :exclude [update find])
  (:require [clojure.spec.alpha :as s]
            [clojure.walk :refer [keywordize-keys]]
            [environ.core :refer [env]]
            [stowaway.core :refer [tag]]
            [stowaway.implicit :as storage :refer [with-storage
                                                   with-transacted-storage]]
            [clj-money.util :refer [update-in-if
                                    ->id]]
            [clj-money.models :as models]
            [clj-money.validation :refer [with-validation]]
            [clj-money.models.entities :as entities]
            [clj-money.models.images :as images]))

(s/def ::id integer?)
(s/def ::entity-name string?)
(s/def ::image-ids (s/coll-of integer?))
(s/def ::user-id integer?)
(s/def ::lt-capital-gains-account string?)
(s/def ::st-capital-gains-account string?)
(s/def ::lt-capital-loss-account string?)
(s/def ::st-capital-loss-account string?)
(s/def ::options (s/keys :opt-un [::lt-capital-gains-account
                                  ::st-capital-gains-account
                                  ::lt-capital-loss-account
                                  ::st-captial-loss-account]))
(s/def ::new-import (s/keys :req-un [::user-id ::entity-name ::image-ids] :opt-un [::options]))
(s/def ::progress map?)
(s/def ::existing-import (s/keys :req-un [::id ::progress]))

(defn- before-save
  [imp]
  (tag imp ::models/import))

(defn- prepare-progress
  [progress]
  (-> progress
      keywordize-keys
      (select-keys [:account
                    :transaction
                    :scheduled-transaction
                    :budget
                    :commodity
                    :price
                    :finished
                    :errors])))

(defn- entity-exists?
  [imp]
  (boolean
   (entities/find-by {:user-id (:user-id imp)
                      :name (:entity-name imp)})))

(defn- after-read
  [imp]
  (when imp
    (-> imp
        (update-in [:progress] prepare-progress)
        (update-in-if [:options] keywordize-keys)
        (assoc :entity-exists? (entity-exists? imp))
        (tag ::models/import))))

(defn create
  [impt]
  (with-storage (env :db)
    (with-validation impt ::new-import []
      (-> impt
          before-save
          storage/create
          after-read))))

(defn search
  ([criteria]
   (search criteria {}))
  ([criteria options]
   (with-storage (env :db)
     (map after-read
          (storage/select (tag criteria ::models/import)
                          (merge {:sort [:created-at]} options))))))

(defn find-by
  ([criteria]
   (find-by criteria {}))
  ([criteria options]
   (first (search criteria (assoc options :limit 1)))))

(defn find
  [import-or-id]
  (find-by {:id (->id import-or-id)}))

(defn update
  [impt]
  (with-storage (env :db)
    (with-validation impt ::existing-import []
      (storage/update impt)
      (find impt))))

(defn delete
  [imp]
  {:pre [imp (map? imp)]}

  (with-transacted-storage (env :db)
    (doseq [image (->> (:image-ids imp)
                       (map images/find)
                       (filter identity))]
      (images/delete image))
    (storage/delete imp)))
