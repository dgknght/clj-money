(ns clj-money.views.accounts.form
  "The form for adding or editing an account.

  The caller owns an atom (or cursor) holding the account and renders
  [account-form account :commodities ... :on-save ... :on-cancel ...].
  :commodities maps the ids of the entity's commodities to the commodities.
  The form saves the account itself and passes the result to :on-save."
  (:require [clojure.string :as string]
            [reagent.ratom :refer [make-reaction]]
            [dgknght.app-lib.inflection :refer [humanize]]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.forms-validation :as v]
            [clj-money.util :refer [id=]]
            [clj-money.components :refer [button]]
            [clj-money.api.accounts :as accounts]
            [clj-money.commodities :as cmdts]
            [clj-money.accounts :refer [account-types
                                        find-by-path]]
            [clj-money.state :refer [accounts
                                     accounts-by-id
                                     +busy
                                     -busy]]
            [clj-money.views.accounts.tags :refer [tags-input]]))

(defn- prepare-for-save
  [{:as account :keys [trading parent-only]}]
  ; TODO: Add logic to turn trading attribute into a system tag
  (cond-> (update-in account [:account/type] keyword)
    trading     (update-in [:account/system-tags] (fnil conj #{}) :trading)
    parent-only (update-in [:account/system-tags] (fnil conj #{}) :parent-only)))

(defn- save
  [account on-save]
  (+busy)
  (accounts/save (prepare-for-save @account)
                 :callback -busy
                 :on-success on-save))

(defn- account-path-field
  [account path & {:as opts}]
  [forms/typeahead-field
   account
   path
   (merge {:search-fn (fn [input callback]
                        (->> @accounts
                             (find-by-path input)
                             callback))
           :caption-fn (comp (partial string/join "/") :account/path)
           :value-fn :id
           :find-fn (fn [id callback]
                      (callback (@accounts-by-id id)))}
          opts)])

(defn account-form
  [account & _opts]
  (let [hide-type? (make-reaction #(-> @account :account/parent :id))]
    (fn [_account & {:keys [commodities on-save on-cancel]}]
      [:form {:no-validate true
              :on-submit (fn [e]
                           (.preventDefault e)
                           (v/validate account)
                           (when (v/valid? account)
                             (save account on-save)))}
       [account-path-field account [:account/parent :id]
        :caption "Parent"
        :on-change (fn [{:account/keys [commodity type]}]
                     (swap! account assoc
                            :account/commodity commodity
                            :account/type type))]
       [forms/select-field account
        [:account/type]
        (map (juxt name humanize) account-types)
        {:hide? hide-type?}]
       [forms/text-field account [:account/name] {:validations #{::v/required}}]
       [forms/typeahead-field
        account
        [:account/commodity :id]
        {:search-fn (fn [input callback]
                      (callback (cmdts/search input (vals commodities))))
         :caption "Commodity"
         :caption-fn cmdts/description
         :find-fn (fn [id callback]
                    (callback (get commodities id)))
         :value-fn :id
         :validations #{::v/required}}]
       [forms/checkbox-field
        account
        [:trading]
        {:caption "Check here if this account is used to trade commodities"
         :form-group-attr {:class (when-not (= :asset (:account/type @account)) "d-none")}}]
       [forms/checkbox-field
        account
        [:parent-only]
        {:caption "Prevent this account from receiving transaction items directly"
         :form-group-attr {:class (when-not (-> @account :account/parent :id) "d-none")}}]
       [forms/checkbox-field
        account
        [:account/hidden]
        {:caption "Hidden"}]
       [account-path-field account [:account/payment-account :id]
        :caption "Payment Account"
        :search-fn (fn [input callback]
                     (->> @accounts
                          (remove #(id= % @account))
                          (find-by-path input)
                          callback))]
       [:fieldset
        [:legend "Tags"]
        [tags-input account]]
       [:div.mt-2
        [button {:html {:type :submit
                        :class "btn-primary"
                        :title "Click here to save the account."}
                 :caption "Save"
                 :icon :check}]
        [button {:html {:on-click on-cancel
                        :class "btn-secondary ms-2"
                        :type :button
                        :title "Click here to return to the list of accounts."}
                 :caption "Cancel"
                 :icon :x}]]])))
