(ns clj-money.state
  (:require [reagent.core :as r]))

(defonce app (r/atom {}))

(defonce entities (r/atom []))

(defonce current-entity (r/atom nil))

