(ns clj-money.web.entities
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [ring.util.response :refer :all]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [cemerick.friend :as friend]
            [clj-money.validation :as validation]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.web.money-shared :refer [grouped-options-for-accounts]])
  (:use clj-money.web.shared))

(defn- entity-row
  "Renders a table row for an entity"
  [entity]
  [:tr
   [:td (:name entity)]
   [:td.col-sm-2
    [:span.btn-group
     (glyph-button :pencil
                   (format "/entities/%s/edit" (:id entity))
                   {:level :info
                    :size :extra-small})
     (glyph-button :remove
                   (format "/entities/%s/delete" (:id entity))
                   {:level :danger
                    :size :extra-small
                    :data-method :post
                    :data-confirm "Are you sure you want to delete this entity?"})]]])

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
  [_]
  (with-layout "Entities" {}
    [:div.row
     [:div.col-md-6
      (entity-table)
      [:a.btn.btn-primary {:href "/entities/new"} "Add"]]]))

(defn new-entity
  "Renders a form for adding a new entity"
  [{{entity :entity} :params}]
   (with-layout "New entity" {}
     [:div.row
      [:div.col-md-6
       [:form {:action "/entities" :method :post}
        (entity-form-fields entity)]]]))

(defn create-entity
  "Creates the entity and redirects to the index on success, 
  or displays the entity from on failuer"
  [{params :params}]
  (let [user (friend/current-authentication)
        entity (entities/create (env :db) (assoc params :user-id (:id user)))]
    (redirect "/entities")))

(defn edit-entity
  "Renders the edit form"
  [{{id :id} :params}]
  (with-layout "Edit entity" {}
    [:div.row
     [:div.col-md-6
      [:form {:action (format "/entities/%s" id) :method :post}
       (let [entity (entities/find-by-id (env :db) (Integer. id))]
         (entity-form-fields entity))]]]))

(defn update
  "Updates the entity and redirects to index on success or
  renders edit on error"
  [{params :params}]
  (let [id (Integer. (:id params))
        entity (entities/find-by-id (env :db) id)
        updated (-> params
                    (select-keys [:name])
                    (assoc :id id))
        result (entities/update (env :db) updated)]
    (if (validation/has-error? result)
      (edit-entity updated)
      (redirect "/entities"))))

(defn delete
  "Removes the entity from the system"
  [{{id :id} :params}]
  (try
    (entities/delete (env :db) (Integer. id))
    (redirect "/entities")
    (catch Exception e
      (index {:alerts [{:type :danger :message (.getMessage e)}]}))))

(defn- monitor-row
  [account]
  [:tr
   [:td (:name account)]
   [:td "&nbsp;"]])

(defn monitors
  ([req] (monitors req {}))
  ([{{entity-id :entity-id} :params} monitor]
   (with-layout "Budget Monitors" {}
     (let [entity (entities/find-by-id (env :db) (Integer. entity-id))]
       [:div.row
        [:div.col-md-6
         [:table.table.table-striped
          [:tr
           [:th "Account"]
           [:th "&nbsp;"]]
          (map monitor-row (->> entity
                                :monitored-account-ids
                                (map #(accounts/find-by-id (env :db) %))))]
         [:form {:action (format "/entities/%s/monitors" (:id entity))
                 :method :post}
          [:div.form-group
           [:label.control-label {:for :account-id} "Add Account"]
           [:select.form-control {:id :account-id :name :account-id}
            (grouped-options-for-accounts (:id entity) {:selected-id (:account-id monitor)})]]
          [:input.btn.btn-primary {:type :submit :value "Add"}]]]]))))

(defn create-monitor
  [{params :params}]
  (let [{:keys [account-id entity-id]} (-> params
                                           (update-in [:entity-id] #(Integer. %))
                                           (update-in [:account-id] #(Integer. %)))
        entity (entities/find-by-id (env :db) entity-id)
        updated (update-in entity
                           [:settings :monitored-account-ids]
                           (fnil #(conj % account-id) []))
        result (entities/update (env :db) updated)]
    (if (validation/has-error? result)
      (monitors {:params params})
      (redirect (format "/entities/%s/accounts" entity-id)))))

(defn delete-monitor
  [{params :params}]
  (let [[account-id entity-id] (->> ((juxt :account-id :entity-id) params)
                                    (map #(Integer. %)))
        entity (entities/find-by-id (env :db) entity-id)
        updated (update-in entity
                           [:monitored-account-ids]
                           #(->> %
                                 (remove (fn [id] (= id account-id)))
                                 (into [])))
        result (entities/update (env :db) updated)]
    (if (validation/valid? result)
      (redirect (format "/entities/%s/accounts" entity-id))
      (monitors entity-id {:new-monitor result}))))
