(ns clj-money.web.grants
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [ring.util.response :refer :all]
            [ring.util.codec :refer [url-encode]]
            [clj-money.authorization :refer [authorize
                                             apply-scope
                                             tag-resource]]
            [clj-money.pagination :as pagination]
            [clj-money.validation :as validation]
            [clj-money.inflection :refer [humanize]]
            [clj-money.models.entities :as entities]
            [clj-money.models.grants :as grants])
  (:use [clj-money.web.shared :refer :all]))

(defn- grant-row
  [grant]
  [:tr
   [:td (:email grant)]
   [:td (:email grant)]])

(defn index
  [{{entity :entity} :params}]
  (with-layout (format "User grants for entity %s" (:name entity)) {}
    ; TODO apply scope
    (let [grants (grants/search (env :db) {:entity-id (:id entity)})]
      [:div.row
       [:div.col-md-6
        [:table.table.table-striped
         [:tr
          [:th "Name"]
          [:th "Email"]]
         (map grant-row grants)]
        [:a.btn.btn-primary
         {:href (format "/entities/%s/grants/new" (:id entity))}
         "Add"]]])))

(defn- permission-key
  [resource-type action]
  (->> [resource-type action]
       (map name)
       (apply format "%s-%s")))

(defn- permission-column
  [grant resource-type]
  [:div.col-sm-4
   [:fieldset
    [:legend (humanize resource-type)]
    (map (fn [action]
           [:div.checkbox
            [:label
             [:input {:type "checkbox"
                      :name (permission-key resource-type action)
                      :checked (grants/has-permission grant resource-type action)}]
             (humanize action)]])
         grants/actions)]])

(defn- permission-checkbox-elements
  [grant]
  (->> grants/resource-types
       (partition 3)
       (map (fn [group]
              [:div.row
               (map #(permission-column grant %) group)]))))

(defn new-grant
  ([{{entity :entity} :params :as req}]
   (new-grant req (-> {:entity-id (:id entity)}
                      (tag-resource :grant)
                      (authorize :new))))
  ([{{entity :entity} :params} grant]
   (with-layout (format "User grants for entity %s" (:name entity)) {}
     [:div.row
      [:div.col-md-6
       (form (format "/entities/%s/grants" (:id entity)) {}
             (email-input-field grant :email)
             (permission-checkbox-elements grant)
             [:button.btn.btn-primary {:type :submit}
              "Save"])]])))

(defn- extract-permissions
  [params]
  (throw (RuntimeException. "Not implemented")))

(defn create
  [{params :params}]
  (let [grant (-> params
                  (select-keys [:entity-id :user-id])
                  (assoc extract-permissions params)
                  (tag-resource :grant)
                  (authorize :create))
        result (grants/create (env :db) grant)]
    (if (validation/has-error? result)
      (new-grant {} result)
      (redirect (format "/entities/%s/grants" (:entity-id result))))))

(defn show
  [req]
  "Show")

(defn edit
  [req]
  "Edit")

(defn update
  [req]
  "Update")

(defn delete
  [req]
  "Delete")
