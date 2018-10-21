(ns clj-money.macros
  (:require [cljs.core.async :as a]))

(defmacro with-retry
  [& body-and-options]
  (let [[options body] (if (map? (first body-and-options))
                         [(first body-and-options)
                          (rest body-and-options)]
                         [{} body-and-options])
        options (merge options {:timeout 500
                                :max-attempts 3})]
    `(a/go
       (a/<! (a/timeout (:timeout ~options)))
       (loop [attempt# 1]
         (let [result# (try
                         (~@body)
                         true ; assuming that the caller doesn't care about the return value
                         (catch js/Error e#
                           false))]
           (when (and (not result#)
                      (< attempt# (:max-attempts ~options)))
             (recur (+ 1 attempt#))))))))
