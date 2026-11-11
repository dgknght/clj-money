(ns clj-money.views.accounts.bulk-edit
  "The form for tagging several accounts at once.

  The caller owns an atom (or cursor) holding the :account-ids of the
  selected accounts and renders
  [bulk-edit-form bulk-edit :on-save ... :on-cancel ...]. The form saves the
  accounts itself, updates them in the app state, and calls :on-save when
  it's done."
  (:require [clojure.string :as string]
            [clojure.set :refer [union]]
            [clojure.core.async :as a]
            [dgknght.app-lib.core :refer [index-by]]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.notifications :as notify]
            [clj-money.components :refer [button]]
            [clj-money.api.accounts :as accounts]
            [clj-money.accounts :refer [multi-save]]
            [clj-money.state :refer [accounts
                                     accounts-by-id
                                     +busy
                                     -busy]]
            [clj-money.views.accounts.tags :refer [tags-input]]))

(defn- put+close
  [ch]
  (fn [x]
    (a/put! ch x)
    (a/close! ch)))

(defn- bulk-save
  [bulk-edit on-save]
  (+busy)
  (let [{:keys [account-ids
                merge-user-tags?]
         :account/keys [user-tags]} @bulk-edit
        find-account @accounts-by-id
        apply-tags (if merge-user-tags?
                     #(update-in % [:account/user-tags] union user-tags)
                     #(assoc % :account/user-tags user-tags))
        ch (->> (if (coll? account-ids)
                  account-ids
                  [account-ids])
                (map (comp apply-tags
                           find-account))
                (multi-save {:process (fn [a ch]
                                        (accounts/save
                                          a
                                          :on-success (put+close ch)
                                          :on-error (put+close ch)))}))]
    (a/go
      (let [{:keys [succeeded errors results]} (a/<! ch)]
        (-busy)
        (on-save)
        (let [by-id (index-by :id results)]
          (swap! accounts (fn [as]
                            (mapv (fn [a]
                                    (if-let [updated (by-id (:id a))]
                                      (assoc a :account/user-tags (:account/user-tags updated))
                                      a))
                                  as))))
        (notify/toast "Update Finished"
                      (str "Updated "
                           succeeded
                           " account(s)."))
        (when (seq? errors)
          (notify/warnf "Some errors were encountered: %s"
                        (string/join "; " errors)))))))

(defn bulk-edit-form
  [bulk-edit & {:keys [on-save on-cancel]}]
  [:form {:no-validate true
          :on-submit (fn [e]
                       (.preventDefault e)
                       (bulk-save bulk-edit on-save))}
   [:fieldset
    [:legend "Tags"]
    [tags-input bulk-edit]]
   [forms/checkbox-field bulk-edit [:merge-user-tags?] {:caption "Keep existing tags"}]
   [button {:html {:title "Click here to apply these changes to the selected accounts."
                   :class "btn-primary"
                   :type :submit}
            :caption "Save"
            :icon :check}]
   [button {:html {:title "Click here to cancel this edit operation."
                   :class "btn-secondary ms-2"
                   :type :button
                   :on-click on-cancel}
            :caption "Cancel"
            :icon :x}]])
