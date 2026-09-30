(ns clj-money.images
  (:refer-clojure :exclude [get])
  (:require [integrant.core :as ig]
            [clj-money.config :refer [env]]
            [digest :refer [sha-1]]))

(def ^:dynamic *storage* nil)

(defprotocol Storage
  (fetch [this uuid] "Retrieves an image by its UUID")
  (stash [this uuid content] "Stores an image that can be retrieved by UUID")
  (close [this] "Releases any resources held by the instance"))

(defmulti reify-storage ::strategy)

(defmethod ig/init-key ::storage
  [_ config]
  (reify-storage config))

(defmethod ig/halt-key! ::storage
  [_ storage]
  (close storage))

; Until every entry point binds *storage* from the Integrant system,
; fall back to a single storage instance shared by the whole process,
; rather than creating a new one on each call.
(def ^:private default-storage
  (delay (reify-storage (:image-storage env))))

(defn storage []
  (or *storage*
      @default-storage))

(defn put
  [content]
  (let [uuid (sha-1 content)]
    (stash (storage) uuid content)
    uuid))

(defn get
  [uuid]
  (fetch (storage) uuid))
