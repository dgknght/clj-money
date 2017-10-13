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
            [clj-money.models.users :as users]
            [clj-money.models.grants :as grants])
  (:use [clj-money.web.shared :refer :all]))

(defn- grant-row
  [grant]
  (let [user (users/find-by-id (env :db) (:user-id grant))]
    [:tr
     [:td (users/full-name user)]
     [:td (:email user)]]))

(defn index
  [{{entity :entity} :params}]
  (with-layout (format "User grants for entity %s" (:name entity)) {}
    (let [grants (grants/search
                   (env :db)
                   (apply-scope {:entity-id (:id entity)} :grant))]
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
                      :checked (grants/has-permission grant resource-type action)
                      :value 1}]
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
     (when (validation/has-error? grant)
       [:div.alert.alert-danger
        (prn-str (validation/error-messages grant))])
     [:div.row
      [:div.col-md-6
       (form (format "/entities/%s/grants" (:id entity)) {}
             (email-input-field grant :email {:autofocus true})
             (permission-checkbox-elements grant)
             [:button.btn.btn-primary {:type :submit}
              "Save"])]])))

(defn- extract-permissions
  [params]
  (->> grants/resource-types
       (mapcat (fn [resource-type]
                 (map #(hash-map :resource-type resource-type
                                 :action %) grants/actions)))
       (map #(assoc % :key (keyword (permission-key (:resource-type %)
                                                    (:action %)))))
       (map #(assoc % :value ((:key %) params)))
       (filter #(= "1" (:value %)))
       (reduce (fn [m {:keys [resource-type action]}]
                 (update-in m
                           [resource-type]
                           #((fnil conj []) % action)))
               {})))

(defn- find-or-create-user
  [email]
  (or (users/find-by-email (env :db) email)
      (throw (RuntimeException. "create user not implemented"))))

(defn create
  [{params :params}]
  (let [grant (-> params
                  (select-keys [:entity-id])
                  (assoc :permissions (extract-permissions params)
                         :user-id (:id (find-or-create-user (:email params))))
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
