(ns clj-money.models.prices
  (:refer-clojure :exclude [update])
  (:require [clojure.spec.alpha :as s]
            [clj-money.util :refer [rev-args]]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.models :as models]
            [clj-money.models.helpers :refer [with-storage
                                              create-fn
                                              update-fn]]
            [clj-money.models.storage :refer [create-price
                                              update-price
                                              select-prices
                                              delete-price
                                              get-setting]]))

(s/def ::commodity-id integer?)
(s/def ::trade-date validation/local-date?)
(s/def ::price decimal?)
(s/def ::id uuid?)
(s/def ::new-price (s/keys :req-un [::commodity-id ::trade-date ::price]))
(s/def ::existing-price (s/keys :req-un [::id ::trade-date ::price] :opt-un [::commodity-id]))

(defn- after-read
  [price & _]
  (when price
    (models/tag price ::models/price)))

(def ^:private coercion-rules
  [(coercion/rule :decimal [:price])
   (coercion/rule :local-date [:trade-date])
   (coercion/rule :integer [:commodity-id])])

(defn- trade-date-exists?
  [storage {:keys [id commodity-id trade-date]}]
  (seq (remove #(and id (= id (:id %)))
               (select-prices
                 storage
                 {:commodity-id commodity-id
                  :trade-date trade-date}
                 {}))))

(defn- validation-rules
  [storage]
  [(validation/create-rule (complement (partial trade-date-exists? storage))
                           [:trade-date]
                           "Trade date must be unique")])

(defn- before-validation
  [price]
  (coercion/coerce coercion-rules price))

(defn- validate
  [storage spec model]
  (->> model
       before-validation
       (validation/validate spec (validation-rules storage))))

(def create
  (create-fn {:create (rev-args create-price)
              :after-read after-read
              :spec ::new-price
              :rules-fn validation-rules
              :coercion-rules coercion-rules}))

(defn search
  ([storage-spec criteria]
   (search storage-spec criteria {}))
  ([storage-spec criteria options]
   (with-storage [s storage-spec]
     (->> (select-prices s criteria options)
          (map after-read)))))

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
  (update-fn {:update (rev-args update-price)
              :rules-fn validation-rules
              :reload reload
              :spec ::existing-price
              :after-read after-read
              :coercion-rules coercion-rules}))

(defn delete
  [storage-spec price]
  (with-storage [s storage-spec]
    (delete-price s price)))

(defn available-date-range
  [storage-spec]
  (with-storage [s storage-spec]
    (->> ["earliest-partition-date"
          "latest-partition-date"]
         (map #(get-setting s %))
         (map read-string))))

(defn most-recent
  ([storage-spec commodity-id]
   (most-recent storage-spec
                commodity-id
                (second (available-date-range storage-spec))))
  ([storage-spec commodity-id as-of]
   (with-storage [s storage-spec]
     (->> (select-prices s
                         {:commodity-id commodity-id
                          :trade-date [:<= as-of]}
                         {:limit 1
                          :sort [[:trade-date :desc]]})
          (sort-by :trade-date <)
          first
          after-read))))
