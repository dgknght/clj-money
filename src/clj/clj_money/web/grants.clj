(ns clj-money.web.grants
  (:refer-clojure :exclude [update])
  (:require [clojure.set :refer [rename-keys]]
            [environ.core :refer [env]]
            [ring.util.response :refer [redirect]]
            [hiccup.core :refer [html]]
            [cemerick.friend :refer [current-authentication]]
            [clj-money.authorization :refer [authorize
                                             apply-scope
                                             tag-resource]]
            [clj-money.validation :as validation]
            [clj-money.inflection :refer [humanize]]
            [clj-money.mailers :as mailers]
            [clj-money.models.entities :as entities]
            [clj-money.models.users :as users]
            [clj-money.models.grants :as grants]
            [clj-money.permissions.grants]
            [clj-money.web.shared :refer [email-input-field form glyph-button with-layout]]))

(defn- grant-row
  [grant]
  (let [user (users/find-by-id (env :db) (:user-id grant))]
    [:tr
     [:td.col-sm-5 (users/full-name user)]
     [:td.col-sm-5 (:email user)]
     [:td.col-sm-2
      [:div.btn-group
       (glyph-button :pencil
                     (format "/grants/%s/edit" (:id grant))
                     {:level :info
                      :size :extra-small
                      :title "Click here to edit permissions for this user."})
       (glyph-button :remove
                     (format "/grants/%s/delete" (:id grant))
                     {:level :danger
                      :size :extra-small
                      :data-method :post
                      :data-confirm "Are your sure you want to remove permissions for this user?"
                      :title "Click here to remove permissions for this user."})]]]))

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
          [:th "Email"]
          [:th "&nbsp;"]]
         (map grant-row grants)]
        [:a.btn.btn-primary
         {:href (format "/entities/%s/grants/new" (:id entity))
          :title "Click here to grant a user access to this entity."}
         "Add"]
        "&nbsp;"
        [:a.btn.btn-default
         {:href "/entities"
          :title "Click here to return to the list of entities."}
         "Back"]]])))

(defn- permission-key
  [resource-type action]
  (->> [resource-type action]
       (map name)
       (apply format "%s-%s")))

(defn- permission-column
  [grant [resource-type actions]]
  [:div.col-sm-4
   [:fieldset
    [:legend (humanize resource-type)]
    (map (fn [action]
           [:div.checkbox
            [:label
             [:input {:type "checkbox"
                      :name (permission-key resource-type action)
                      :checked (grants/has-permission? grant resource-type action)
                      :value 1}]
             (humanize action)]])
         actions)]])

(defn- permission-checkbox-elements
  [grant]
  (->> grants/available-permissions
       (partition-all 3)
       (map (fn [group]
              [:div.row
               (map #(permission-column grant %) group)]))))

(defn- form-fields
  [grant]
  (html
    (if (:id grant)
      [:h3
       (-> grant :user users/full-name)]
      (email-input-field grant :user {:autofocus true
                                      :format-fn :email}))
    (permission-checkbox-elements grant)
    [:button.btn.btn-primary {:type :submit
                              :title "Click here to save the grant."}
     "Save"]
    "&nbsp;"
    [:a.btn.btn-default {:href (format "/entities/%s/grants" (:entity-id grant))
                         :title "Click here to return to the list of grants."}
     "Back"]))

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
             (form-fields {:entity-id (:id entity)}))]])))

(defn- extract-permissions
  [params]
  (->> grants/available-permissions
       (mapcat (fn [[resource-type actions]]
                 (map #(hash-map :resource-type resource-type
                                 :action %) actions)))
       (map #(assoc % :key (keyword (permission-key (:resource-type %)
                                                    (:action %)))))
       (map #(assoc % :value ((:key %) params)))
       (filter #(= "1" (:value %)))
       (reduce (fn [m {:keys [resource-type action]}]
                 (update-in m
                           [resource-type]
                           #((fnil conj #{}) % action)))
               {})))

(defn- create-and-invite-new-user
  [user-attr]
  (let [user (users/create (env :db) user-attr)
        token (users/create-password-reset-token (env :db) user)
        url (format "%s://%s/password/%s")]
    (mailers/invite-user {:url url
                          :from-user current-authentication
                          :to-user user
                          :token token})))

(defn- find-or-create-user
  [user]
  (or (users/find-by-email (env :db) (:email user))
      (create-and-invite-new-user user)))

(defn create
  [{params :params}]
  (let [grant (-> params
                  (select-keys [:entity-id])
                  (assoc :permissions (extract-permissions params)
                         :user-id (:id (find-or-create-user (-> params
                                                                (select-keys [:user])
                                                                (rename-keys {:user :email})))))
                  (tag-resource :grant)
                  (authorize :create))
        result (grants/create (env :db) grant)]
    (if (validation/has-error? result)
      (new-grant {} result)
      (redirect (format "/entities/%s/grants" (:entity-id result))))))

(defn edit
  ([{{:keys [id]} :params :as req}]
   (edit req (as-> (grants/find-by-id (env :db) id) g
               (authorize g :edit)
               (assoc g :user (users/find-by-id (env :db) (:user-id g))))))
  ([_ grant]
   (let [entity (entities/find-by-id (env :db) (:entity-id grant))]
     (with-layout (format "User grants for entity %s" (:name entity)) {}
       (when (validation/has-error? grant)
         [:div.alert.alert-danger
          (prn-str (validation/error-messages grant))])
       [:div.row
        [:div.col-md-6
         (form (format "/grants/%s" (:id grant)) {}
               (form-fields grant))]]))))

(defn update
  [{{id :id :as params} :params}]
  (let [grant (-> (grants/find-by-id (env :db) id)
                  (authorize :update)
                  (assoc :permissions (extract-permissions params)))
        updated (grants/update (env :db) grant)]
    (if (validation/has-error? updated)
      (edit nil (assoc grant :user (users/find-by-id (env :db) (:user-id grant))))
      (redirect (format "/entities/%s/grants" (:entity-id grant))))))

(defn delete
  [{{id :id} :params}]
  (let [grant (as-> id v
                (grants/find-by-id (env :db) v)
                (authorize v :delete))]
    (grants/delete (env :db) (:id grant))
    (redirect (format "/entities/%s/grants" (:entity-id grant)))))
