(ns clj-money.models.imports
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.spec :as s]
            [clojure.java.io :as io]
            [clj-money.validation :as validation]
            [clj-money.coercion :as coercion]
            [clj-money.models.helpers :refer [with-storage
                                              create-fn
                                              update-fn]]
            [clj-money.models.storage :refer [create-import]])
  (:import java.io.ByteArrayOutputStream))

(defn- before-save
  [storage import]
  (-> import
      (assoc :content (with-open [out (ByteArrayOutputStream.)]
                        (io/copy (:source-file import) out)
                        (.toByteArray out)))
      (dissoc :source-file)))

(def create
  (create-fn {:create create-import
              :before-save before-save}))
