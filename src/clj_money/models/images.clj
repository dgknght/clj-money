(ns clj-money.models.images
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec :as s]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.models.helpers :refer [with-storage
                                              create-fn
                                              update-fn]]
            [clj-money.models.storage :refer [create-image]]))

(s/def ::image (s/keys :req-un []))

(def create
  (create-fn {:create create-image
              :spec ::image}))
