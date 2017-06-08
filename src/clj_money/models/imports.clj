(ns clj-money.models.imports
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec :as s]
            [clj-money.io :as io]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.models.helpers :refer [with-storage
                                              create-fn
                                              update-fn]]
            [clj-money.models.storage :refer [create-import]]))

(defn- before-save
  [storage import]
  (update-in import [:source-file] #(io/read-bytes %)))

(def create
  (create-fn {:create create-import
              :before-save before-save}))
