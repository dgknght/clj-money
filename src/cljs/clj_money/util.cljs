(ns clj-money.util
  (:require [goog.string :as gstring]))

; TODO Still need a working solution here
#_(def space [:span {:dangerouslySetInnerHtml {:__html "&nbsp;"}}])
#_(def space (gstring/unescapeEntities " "))
