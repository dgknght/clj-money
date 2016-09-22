(ns clj-money.web.entities
  (:require [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [cemerick.friend :as friend]
            [clj-money.models.entities :as entities])
  (:use clj-money.web.shared))

(defn- entity-row
  "Renders a table row for an entity"
  [entity]
  [:tr
   [:td (:name entity)]])

(defn index
  "Renders the list of entities that belong to the currently
  authenticated user"
  []
  (layout
    "Entities" {}
    [:table.table.table-striped
     [:tr
      [:th name]]
     (let [user (friend/current-authentication)
           ]
       (map entity-row (entities/select (:id user))))]))
