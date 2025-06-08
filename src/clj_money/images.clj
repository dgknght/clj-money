(ns clj-money.images
  (:refer-clojure :exclude [get])
  (:require [clj-money.config :refer [env]]
            [digest :refer [sha-1]]))

(defprotocol Storage
  (fetch [this uuid] "Retrieves an image by its UUID")
  (stash [this uuid content] "Stores an image that can be retrieved by UUID"))

(defmulti reify-storage ::strategy)

(defn- storage []
  (reify-storage (:image-storage env)))

(defn put
  [content]
  (let [uuid (sha-1 content)]
    (stash (storage) uuid content)
    uuid))

(defn get
  [uuid]
  (fetch (storage) uuid))
