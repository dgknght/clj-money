(ns clj-money.web.entities
  (:require [environ.core :refer [env]]
            [ring.util.response :refer :all]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [cemerick.friend :as friend]
            [clj-money.models.entities :as entities])
  (:use clj-money.web.shared))

(defn- entity-row
  "Renders a table row for an entity"
  [entity]
  [:tr
   [:td (:name entity)]
   [:td
    [:a.btn.btn-xs.btn-info {:href (format "/entities/%s/edit" (:id entity))}
     [:span.glyphicon.glyphicon-pencil {:aria-hidden true} "Edit"]]]])

(defn- entity-table
  "Renders the table of entities belonging to the
  authenticated user"
  []
  [:table.table.table-striped
   [:tr
    [:th "Name"]
    [:th "&nbsp;"]]
   (let [user (friend/current-authentication)
         entities (entities/select (env :db) (:id user))]
     (map entity-row entities))])

(defn- entity-form-fields
  "Renders form fields for the entity"
  [entity]
  (html
    (text-input-field entity :name {:autofocus true
                                    :maxlength 100})
    [:input.btn.btn-primary {:type :submit :value "Save"}]))

(defn index
  "Renders the list of entities that belong to the currently
  authenticated user"
  []
  (layout
    "Entities" {}
    [:div.row
     [:div.col-md-6
      (entity-table)
      [:a.btn.btn-primary {:href "/entities/new"} "Add"]]]))

(defn new-entity
  "Renders a form for adding a new entity"
  ([] (new-entity {}))
  ([entity]
   (layout
     "New entity" {}
     [:div.row
      [:div.col-md-6
       [:form {:action "/entities" :method :post}
        (entity-form-fields entity)]]])))

(defn create-entity
  "Creates the entity and redirects to the index on success, 
  or displays the entity from on failuer"
  [{entity-name :name :as params}]
  (let [user (friend/current-authentication)
        entity (entities/create (env :db) {:name entity-name
                                           :user-id (:id user)})]
    (redirect "/entities")))

(defn edit-entity
  "Renders the edit form"
  [id]
  (layout
    "Edit entity" {}
    [:div.row
     [:div.col-md-6
      [:form {:action (format "/entities/%s" id) :method :post}
       (let [entity (entities/find-by-id (env :db) (Integer. id))]
         (entity-form-fields entity))]]]))
