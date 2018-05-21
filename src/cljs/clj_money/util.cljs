(ns clj-money.util
  (:require [goog.string :as gstring]))

(defn space
  "Renders an HTML non-breakable space."
  []
  [:span {:dangerouslySetInnerHTML {:__html "&nbsp;"}}])
