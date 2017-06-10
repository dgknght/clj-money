(ns clj-money.models.images
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec :as s]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.models.helpers :refer [with-storage
                                              create-fn
                                              update-fn]]
            [clj-money.models.storage :refer [create-image
                                              select-images]]))

(s/def ::user-id integer?)
(s/def ::original-filename validation/non-empty-string?)
(s/def ::body-hash validation/non-empty-string?)
(s/def ::body bytes?)
(s/def ::image (s/keys :req-un [::user-id
                                ::original-filename
                                ::body-hash
                                ::body]))

(defn- body-hash-is-unique?
  [storage {:keys [body-hash user-id]}]
  (->> (select-images storage {:user-id user-id})
       (filter #(= (:body-hash %) body-hash))
       empty?))

(defn- validation-rules
  [storage]
  [(validation/create-rule (partial body-hash-is-unique? storage)
                           [:body-hash]
                           "The image content must be unique")])

(def create
  (create-fn {:create create-image
              :spec ::image
              :rules-fn validation-rules}))
