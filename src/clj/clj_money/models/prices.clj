(ns clj-money.models.prices
  (:refer-clojure :exclude [update])
  (:require [clojure.spec.alpha :as s]
            [stowaway.core :as storage :refer [with-storage]]
            [clj-money.util :refer [rev-args]]
            [clj-money.x-platform.util :refer [deep-update-in-if]]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.models :as models]
            [clj-money.models.settings :as settings]
            [clj-money.models.helpers :refer [create-fn
                                              update-fn]]
            [clj-money.models.date-helpers :refer [parse-date-range]]))

(s/def ::commodity-id integer?)
(s/def ::trade-date validation/local-date?)
(s/def ::price decimal?)
(s/def ::id uuid?)
(s/def ::new-price (s/keys :req-un [::commodity-id ::trade-date ::price]))
(s/def ::existing-price (s/keys :req-un [::id ::trade-date ::price] :opt-un [::commodity-id]))

(defn- after-read
  [price & _]
  (when price
    (storage/tag price ::models/price)))

(def ^:private coercion-rules
  [(coercion/rule :decimal [:price])
   (coercion/rule :local-date [:trade-date])
   (coercion/rule :integer [:commodity-id])])

(defn- prepare-criteria
  [criteria]
  (-> criteria
      (deep-update-in-if :trade-date parse-date-range)
      (storage/tag ::models/price)))

(defn search
  ([storage-spec criteria]
   (search storage-spec criteria {}))
  ([storage-spec criteria options]
   (with-storage [s storage-spec]
     (->> (storage/select s (prepare-criteria criteria) options)
          (map after-read)))))

(defn- trade-date-exists?
  [storage {:keys [id commodity-id trade-date]}]
  (seq (remove #(and id (= id (:id %)))
               (search
                 storage
                 {:commodity-id commodity-id
                  :trade-date [:between trade-date trade-date]}))))

(defn- validation-rules
  [storage]
  [(validation/create-rule (complement (partial trade-date-exists? storage))
                           [:trade-date]
                           "Trade date must be unique")])

(defn- before-validation
  [price]
  (coercion/coerce coercion-rules price))

(defn- before-save
  [price & _]
  (storage/tag price ::models/price))

(defn- validate
  [storage spec model]
  (->> model
       before-validation
       (validation/validate spec (validation-rules storage))))

(def create
  (create-fn {:create (rev-args storage/create)
              :before-save before-save
              :after-read after-read
              :spec ::new-price
              :rules-fn validation-rules
              :coercion-rules coercion-rules}))

(defn find-by-id
  [storage-spec id trade-date]
  (first (search storage-spec
                 {:id id
                  :trade-date trade-date}
                 {:limit 1})))

(defn reload
  [storage-spec price]
  (find-by-id storage-spec (:id price) (:trade-date price)))

(def update
  (update-fn {:update (rev-args storage/update)
              :rules-fn validation-rules
              :reload reload
              :spec ::existing-price
              :after-read after-read
              :coercion-rules coercion-rules}))

(defn delete
  [storage-spec price]
  (with-storage [s storage-spec]
    (storage/delete s price)))

(defn most-recent
  ([storage-spec commodity-id]
   (most-recent storage-spec
                commodity-id
                (settings/get storage-spec "latest-partition-date")))
  ([storage-spec commodity-id as-of]
   (with-storage [s storage-spec]
     (->> (search s
                  {:commodity-id commodity-id
                   :trade-date [:<= as-of]}
                  {:limit 1
                   :sort [[:trade-date :desc]]})
          (sort-by :trade-date <)
          first
          after-read))))
