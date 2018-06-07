(ns clj-money.util
  (:require [goog.string :as gstring]
            [clojure.string :as string]))

(defn space
  "Renders an HTML non-breakable space."
  []
  [:span {:dangerouslySetInnerHTML {:__html "&nbsp;"}}])

(defn- stringify
  [value]
  (if (keyword? value)
    (name value)
    (str value)))

(defn path
  [& segments]
  (str "/" (->> segments
                (map stringify)
                (string/join "/"))))
