(ns clj-money.dnd
  (:require [cljs.pprint :refer [pprint]]))

(defn- data-transfer
  [e]
  (.-dataTransfer e))

(defn- files
  [dt]
  (.-files dt))

(defn- length
  [list]
  (.-length list))

(defn- item
  [list index]
  (.item list index))

(defn data-files
  [event]
  (let [file-list (-> event data-transfer files)
        file-count (length file-list)]
    (mapv #(item file-list %) (range file-count))))
