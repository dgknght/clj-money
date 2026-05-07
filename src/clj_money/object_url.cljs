(ns clj-money.object-url
  (:require [clojure.core.async :as a]))

(defn- revoke
  [url]
  (js/URL.revokeObjectURL url))

(defn create
  [blob]
  (let [url (js/URL.createObjectURL blob)]
    (a/go
      (a/<! (a/timeout 60000))
      (revoke url))
    url))
