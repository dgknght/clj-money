(ns clj-money.models.attachments
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec :as s]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.models.helpers :refer [with-storage
                                              with-transacted-storage]]
            #_[clj-money.models.storage :refer [create-attachment]]))

(defn create
  [storage-spec attachment]
  )

(defn search
  [storage-spec criteria]
  )
