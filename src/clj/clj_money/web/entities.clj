(ns clj-money.web.entities
  (:refer-clojure :exclude [update])
  (:require [environ.core :refer [env]]
            [ring.util.response :refer [redirect]]
            [hiccup.core :refer [html]]
            [cemerick.friend :as friend]
            [clj-money.authorization :refer [authorize
                                             tag-resource]]
            [clj-money.validation :as validation]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.web.money-shared :refer [grouped-options-for-accounts]]
            [clj-money.web.shared :refer [form
                                          glyph-button
                                          text-input-field
                                          with-layout]]))

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
                    :size :extra-small
                    :title "Click here to edit this entity"})
     (glyph-button :user
                   (format "/entities/%s/grants" (:id entity))
                   {:size :extra-small
                    :title "Click here manage other users' access to this entity"})
     (glyph-button :remove
                   (format "/entities/%s/delete" (:id entity))
                   {:level :danger
                    :size :extra-small
                    :title "Click here to delete this entity. NB This action cannot be undone."
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
    (text-input-field entity :name {:autofocus "true"
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
      [:a.btn.btn-primary {:href "/entities/new"} "Add"]
      "&nbsp;"
      [:a.btn.btn-default {:href "/imports/new"} "Import"]]]))

(defn new-entity
  "Renders a form for adding a new entity"
  [{{entity :entity} :params}]
   (with-layout "New entity" {}
     [:div.row
      [:div.col-md-6
       (form "/entities" {}
             (entity-form-fields entity))]]))

(defn create-entity
  "Creates the entity and redirects to the index on success, 
  or displays the entity from on failuer"
  [{params :params}]
  (let [user (friend/current-authentication)
        entity (-> params
                   (select-keys [:name]) ; TODO move allowed attributes to authorization layer
                   (assoc :user-id (:id user))
                   (tag-resource :entity)
                   (authorize :create))
        result (entities/create (env :db) entity)]
    (redirect (format "/entities/%s/accounts" (:id result)))))

(defn- load-and-authorize
  [id action]
  (authorize (entities/find-by-id (env :db) id) action))

(defn edit-entity
  "Renders the edit form"
  ([req] (edit-entity req nil))
  ([{{id :id} :params} entity]
   (with-layout "Edit entity" {}
     [:div.row
      [:div.col-md-6
       (form (format "/entities/%s" id) {}
             (let [entity (or entity (load-and-authorize id :edit))]
               (entity-form-fields entity)))]])))

(defn update
  "Updates the entity and redirects to index on success or
  renders edit on error"
  [{{id :id :as params} :params}]
  (let [entity (load-and-authorize id :update)
        updated (merge entity (select-keys params [:name])) ; TODO Add list of allowed attributes to authorization layer
        result (entities/update (env :db) updated)]
    (if (validation/has-error? result)
      (edit-entity {} result)
      (redirect "/entities"))))

(defn delete
  "Removes the entity from the system"
  [{{id :id} :params}]
  (try
    (entities/delete (env :db)
                     (:id (load-and-authorize id :delete)))
    (redirect "/entities")
    (catch Exception e
      (index {:alerts [{:type :danger :message (.getMessage e)}]}))))

(defn- monitor-row
  [entity account]
  [:tr
   [:td (:name account)]
   [:td
    (form (format "/entities/%s/monitors/%s/delete"
                  (:id entity)
                  (:id account)) {}
          [:button.btn.btn-xs.btn-danger {:title "Click here to remove this budget monitor."}
           [:span.glyphicon.glyphicon-remove {:aria-hidden true}]])]])

(defn monitors
  ([req] (monitors req {}))
  ([{{entity :entity} :params} monitor]
   (authorize entity :edit)
   (with-layout "Budget Monitors" {}
     [:div.row
      [:div.col-md-3
       [:table.table.table-striped
        [:tr
         [:th "Account"]
         [:th "&nbsp;"]]
        (map #(monitor-row entity %)
             (->> entity
                  :settings
                  :monitored-account-ids
                  (map #(accounts/find-by-id (env :db) %))))]
       (form (format "/entities/%s/monitors" (:id entity)) {}
             [:div.form-group
              [:label.control-label {:for :account-id} "Add Account"]
              [:select.form-control {:id :account-id :name :account-id}
               (grouped-options-for-accounts (:id entity) {:selected-id (:account-id monitor)})]]
             [:input.btn.btn-primary {:type :submit :value "Add"}])]])))

(defn create-monitor
  [{{:keys [account-id entity-id] :as params} :params}]
  (let [entity (authorize (entities/find-by-id (env :db) entity-id) :update)
        updated (update-in entity
                           [:settings :monitored-account-ids]
                           (fnil #(conj % account-id) []))
        result (entities/update (env :db) updated)]
    (if (validation/has-error? result)
      (monitors {:params params})
      (redirect (format "/entities/%s/accounts" entity-id)))))

(defn delete-monitor
  [{{:keys [account-id entity]} :params}]
  (authorize entity :update)
  (let [updated (update-in entity
                           [:settings :monitored-account-ids]
                           #(->> %
                                 (remove #{account-id})
                                 (into [])))
        result (entities/update (env :db) updated)]
    (if (validation/has-error? result)
      (monitors (:id entity) {:new-monitor result})
      (redirect (format "/entities/%s/accounts" (:id entity))))))
