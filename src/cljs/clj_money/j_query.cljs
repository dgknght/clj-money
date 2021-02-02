(ns clj-money.j-query
  (:require [clj-money.macros :refer-macros [call]]))

(defn- match
  [selector]
  (call js/window :j-query selector))

(defn toast
  [selector]
  (call (match selector) :toast "show"))

(defn tool-tip
  [selector]
  (call (match selector) :tooltip))

(defn popover
  [selector opts]
  (call (match selector) :popover (clj->js opts)))
