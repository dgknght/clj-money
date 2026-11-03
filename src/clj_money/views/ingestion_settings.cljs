(ns clj-money.views.ingestion-settings
  "The off-canvas drawer in which the user edits the entity settings
  used to read receipt images, and the mailbox checked for emailed
  receipts."
  (:require [clojure.string :as string]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.web :refer [format-date-time]]
            [clj-money.util :as util]
            [clj-money.icons :refer [icon
                                     icon-with-text]]
            [clj-money.state :refer [current-entity
                                     accounts
                                     accounts-by-id
                                     +busy
                                     -busy]]
            [clj-money.accounts :refer [find-by-path]]
            [clj-money.api.entities :as entities]
            [clj-money.api.mailboxes :as mailboxes]))

(defn- ->draft
  "Extracts the settings to be edited from the entity."
  [{{:settings/keys [payment-methods expense-accounts expense-hints]} :entity/settings}]
  {:payment-methods (set payment-methods)
   :expense-accounts (set expense-accounts)
   :expense-hints (string/join "\n" expense-hints)})

(defn- <-draft
  "Applies the edited settings to the entity."
  [entity {:keys [payment-methods expense-accounts expense-hints]}]
  (update-in entity
             [:entity/settings]
             assoc
             :settings/payment-methods payment-methods
             :settings/expense-accounts expense-accounts
             ; one hint per line
             :settings/expense-hints (->> (string/split-lines (or expense-hints ""))
                                          (map string/trim)
                                          (remove string/blank?)
                                          vec)))

(def ^:private new-mailbox
  #:mailbox{:port 993
            :folder "INBOX"})

(defn- ->mailbox-draft
  [mailbox]
  (-> (merge new-mailbox mailbox)
      (update-in [:mailbox/allowed-senders] #(string/join "\n" %))))

(defn- <-mailbox-draft
  [mailbox]
  (update-in mailbox
             [:mailbox/allowed-senders]
             ; one sender per line
             #(->> (string/split-lines (or % ""))
                   (map string/trim)
                   (remove string/blank?)
                   vec)))

(defn- load-mailbox
  [draft]
  (mailboxes/select
    :on-success #(swap! draft assoc :mailbox (->mailbox-draft (first %)))))

(defn- save-mailbox
  "Saves the mailbox, if one has been set up or is being set up"
  [{{:keys [id] :mailbox/keys [host] :as mailbox} :mailbox}]
  (when (or id (not (string/blank? host)))
    (+busy)
    (mailboxes/save (<-mailbox-draft mailbox)
                    :callback -busy)))

(defn- save
  [draft]
  (let [updated (<-draft @current-entity @draft)]
    (+busy)
    (entities/save updated
                   :callback -busy
                   :on-success #(reset! current-entity updated))
    (save-mailbox @draft)))

(defn- remove-mailbox
  [draft]
  (when (js/confirm "Are you sure you want to stop reading receipts from this mailbox?")
    (+busy)
    (mailboxes/delete (:mailbox @draft)
                      :callback -busy
                      :on-success #(swap! draft assoc :mailbox (->mailbox-draft nil)))))

(defn- mailbox-status
  [{:mailbox/keys [checked-at error]}]
  (cond
    error [:div.alert.alert-danger.py-2 "The last check failed: " error]
    checked-at [:div.form-text.mb-2 "Last checked " (format-date-time checked-at)]))

(defn- mailbox-section
  "Renders the fields for the mailbox checked for emailed receipts"
  [draft]
  (let [mailbox (get-in @draft [:mailbox])]
    [:<>
     [:h4.mt-4 "Mailbox"]
     [:p.form-text
      "Receipts sent to this mailbox are read every few minutes, and wait for your review. "
      "Use an app password, and a folder or label just for receipts."]
     (when (:id mailbox)
       [mailbox-status mailbox])
     [forms/text-field draft [:mailbox :mailbox/host] {:caption "IMAP Server"
                                                        :html {:placeholder "imap.gmail.com"}}]
     [forms/integer-field draft [:mailbox :mailbox/port] {:caption "Port"}]
     [forms/text-field draft [:mailbox :mailbox/username] {:caption "User Name"
                                                            :html {:auto-complete "off"}}]
     [forms/text-field draft [:mailbox :mailbox/password]
      {:caption "Password"
       :type :password
       :html {:auto-complete "new-password"
              :placeholder (when (:id mailbox) "Unchanged")}}]
     [forms/text-field draft [:mailbox :mailbox/folder] {:caption "Folder"}]
     [:div.mb-3
      [:label.form-label {:for "mailbox-allowed-senders"} "Allowed Senders"]
      [forms/textarea-elem draft [:mailbox :mailbox/allowed-senders]
       {:html {:rows 3
               :id "mailbox-allowed-senders"
               :placeholder "receipts@store.com\n@example.com"}}]
      [:div.form-text "Only messages from these addresses, or domains starting with @, one per line, are read. Leave it empty to read every message."]]
     (when (:id mailbox)
       [:button.btn.btn-sm.btn-outline-danger.mb-3
        {:type :button
         :title "Click here to stop reading receipts from this mailbox."
         :on-click #(remove-mailbox draft)}
        (icon-with-text :x-circle "Remove Mailbox" :size :small)])]))

(defn- account-path
  [{:keys [id]}]
  (some->> (@accounts-by-id id)
           :account/path
           (string/join "/")))

(defn- account-list
  "Renders the accounts chosen for the setting at k, each with a button to
  remove it, and a typeahead to choose another from the accounts that
  satisfy pred."
  [draft k {:keys [caption help pred]}]
  (let [new-key (keyword (str "new-" (name k)))]
    ; position-relative keeps the absolutely-positioned search results
    ; anchored to the input when the drawer body scrolls
    [:div.mb-3.position-relative
     [:label.form-label {:for (str "ingestion-settings-" (name new-key))}
      caption]
     (when-let [refs (seq (get-in @draft [k]))]
       [:ul.list-group.mb-2
        (->> refs
             (sort-by account-path)
             (map (fn [ref]
                    ^{:key (str (name k) "-" (:id ref))}
                    [:li.list-group-item.d-flex.justify-content-between.align-items-center
                     (account-path ref)
                     [:button.btn.btn-sm.btn-link.text-danger
                      {:type :button
                       :title "Click here to remove this account."
                       :on-click #(swap! draft update-in [k] disj ref)}
                      (icon :x :size :small)]]))
             doall)])
     [forms/typeahead-input
      draft
      [new-key]
      {:html {:id (str "ingestion-settings-" (name new-key))
              :placeholder "Add an account"}
       :search-fn (fn [input callback]
                    (callback (find-by-path input (filter pred @accounts))))
       :find-fn (fn [account callback]
                  (callback (@accounts-by-id (:id account))))
       :caption-fn #(string/join "/" (:account/path %))
       :on-change (fn [account]
                    (when (map? account)
                      (swap! draft #(-> %
                                        (update-in [k] conj (util/->entity-ref account))
                                        (dissoc new-key)))))}]
     [:div.form-text help]]))

(defn toggle
  "Renders a button that opens the receipt reading settings drawer with
  the given DOM id, and loads the entity's current settings into the
  draft."
  [id draft]
  [:button.btn.btn-secondary
   {:type :button
    :data-bs-toggle "offcanvas"
    :data-bs-target (str "#" id)
    :aria-controls id
    :title "Click here to change how receipt images are read."
    :on-click (fn []
                (reset! draft (assoc (->draft @current-entity)
                                     :mailbox (->mailbox-draft nil)))
                (load-mailbox draft))}
   (icon :gear :size :small)])

(defn drawer
  "Renders an off-canvas drawer, docked to the right side of the screen,
  in which the settings used to read receipt images are edited."
  [id draft]
  [:div.offcanvas.offcanvas-end {:id id :tab-index -1}
   [:div.offcanvas-header
    [:h3 "Receipt Settings"]
    [:button.btn-close.text-reset {:data-bs-dismiss "offcanvas"
                                   :aria-label "Close"}]]
   [:div.offcanvas-body
    (when @draft
      [:<>
       [account-list draft :payment-methods
        {:caption "Payment Methods"
         :help "The accounts that can be chosen for the payment."
         :pred (comp #{:asset :liability} :account/type)}]
       [account-list draft :expense-accounts
        {:caption "Expense Accounts"
         :help "The accounts that can be chosen for the items. If none are chosen, any expense account without children can be."
         :pred (comp #{:expense} :account/type)}]
       [:div.mb-3
        [:label.form-label {:for "expense-hints"} "Expense Hints"]
        [forms/textarea-elem draft [:expense-hints] {:html {:rows 6}}]
        [:div.form-text "Guidelines for choosing expense accounts, one per line."]]
       [mailbox-section draft]
       [:div.mt-3
        [:button.btn.btn-primary
         {:type :button
          :data-bs-dismiss "offcanvas"
          :title "Click here to save these settings."
          :on-click #(save draft)}
         (icon-with-text :check "Save")]
        [:button.btn.btn-secondary.ms-2
         {:type :button
          :data-bs-dismiss "offcanvas"
          :title "Click here to discard these changes."}
         (icon-with-text :x "Cancel")]]])]])
