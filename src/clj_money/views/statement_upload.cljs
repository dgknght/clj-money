(ns clj-money.views.statement-upload
  "Uploads a statement and follows the read of it, for the statements page
  and the reconciliation form"
  (:require [clojure.string :as string]
            [cljs.pprint :refer [pprint]]
            [reagent.core :as r]
            [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.forms :as forms]
            [dgknght.app-lib.notifications :as notify]
            [clj-money.components :refer [button]]
            [clj-money.icons :refer [icon-with-text]]
            [clj-money.state :refer [accounts
                                     accounts-by-id]]
            [clj-money.accounts :refer [find-by-path]]
            [clj-money.dates :as dates]
            [clj-money.cached-accounts :refer [fetch-accounts
                                               push-transaction-date!]]
            [clj-money.api.statement-ingestions :as si]))

(def poll-interval
  "How long to wait, in milliseconds, between checks on the read of a
  statement."
  2000)

(defn- following?
  "Returns true if the reading state is still waiting on the specified
  ingestion."
  [reading {:keys [id]}]
  (= id (:id @reading)))

(defn- complete
  [reading
   {:statement-ingestion/keys [warnings account]
    {:keys [period-start period-end]} :statement-ingestion/statement}
   on-complete]
  (reset! reading nil)
  (doseq [w warnings]
    (notify/warn w))
  ; transactions may have been created for the statement, which extends the
  ; transaction date ranges of the accounts they touch
  (fetch-accounts)
  (on-complete (reduce (fn [a d]
                         (if d
                           (push-transaction-date! a (dates/unserialize-local-date d))
                           a))
                       (or (get @accounts-by-id (:id account))
                           account)
                       [period-start period-end])))

(defn- await-ingestion
  "Follows the ingestion until it's finished. callbacks is a map of
  :on-complete, called with the account reconciled, and :on-read-failure,
  called with an ingestion whose statement was read but couldn't be
  reconciled."
  [reading ingestion {:keys [on-complete on-read-failure] :as callbacks}]
  (js/setTimeout
    (fn []
      (when (following? reading ingestion)
        (si/get ingestion
                :on-failure (fn [_] (reset! reading nil))
                :on-success
                (fn [{:statement-ingestion/keys [status error] :as updated}]
                  (when (following? reading updated)
                    (case status
                      :complete
                      (complete reading updated on-complete)

                      :failed
                      (if (:statement-ingestion/statement updated)
                        ; the statement was read, but couldn't be
                        ; reconciled, so the user can correct it and try
                        ; again
                        (do
                          (reset! reading (assoc updated ::callbacks callbacks))
                          (when on-read-failure
                            (on-read-failure updated)))
                        (do
                          (notify/dangerf "Unable to read the statement: %s" error)
                          (reset! reading nil)))

                      ; the account has a reconciliation in progress that
                      ; doesn't match the statement, which the user can
                      ; choose to replace
                      :conflict
                      (reset! reading (assoc updated ::callbacks callbacks))

                      (:processing :pending)
                      (do
                        (reset! reading updated)
                        (await-ingestion reading updated callbacks))

                      (do
                        (notify/dangerf "Unrecognized ingestion status: %s" (pr-str status))
                        (reset! reading nil))))))))
    poll-interval))

(defn read-statement
  "Uploads the statement and follows the read of it, keeping the
  statement-ingestion in the reading atom while it's in progress. When the
  account is given, the statement is reconciled against it. When the read
  is finished, on-complete is called with the account the statement was
  reconciled against. If the statement is read but can't be reconciled,
  on-read-failure is called with the ingestion, which holds what was read."
  [file reading & {:keys [account on-complete on-read-failure]}]
  (reset! reading {:id ::uploading})
  (si/create file
             :account account
             :on-failure (fn [_] (reset! reading nil))
             :on-success (fn [ingestion]
                           (reset! reading ingestion)
                           (await-ingestion reading
                                            ingestion
                                            {:on-complete on-complete
                                             :on-read-failure on-read-failure}))))

(defn statement-input
  "A button that opens the file chooser and reads the chosen statement"
  [reading & {:as opts :keys [id caption]}]
  (let [id (or id "statement")]
    [:<>
     ; the button is the input's label, which opens its file chooser, so the
     ; input itself, with its \"Choose File\" text, isn't shown
     [:label.btn.btn-secondary
      {:for id
       :class (when @reading "disabled")
       :title "Click here to choose a PDF of a bank or credit card statement."}
      (icon-with-text :file-earmark-arrow-up (or caption "Statement"))]
     [:input.d-none
      {:id id
       :type :file
       :accept "application/pdf"
       :disabled (boolean @reading)
       :on-change (fn [e]
                    (let [target (.-target e)]
                      (when-let [file (some-> target .-files (aget 0))]
                        (apply read-statement
                               file
                               reading
                               (mapcat identity (dissoc opts :id :caption)))
                        ; allow the same file to be chosen again
                        (set! (.-value target) ""))))}]]))

(defn- path-of
  [account]
  (string/join "/" (:account/path account)))

(defn- statement-account
  "Returns the account the statement is for: the one it was uploaded for,
  or the one whose path the model read from it"
  [{:statement-ingestion/keys [account]
    {path :account} :statement-ingestion/statement}]
  (if account
    (get @accounts-by-id (:id account) account)
    (->> @accounts
         (filter #(= path (path-of %)))
         first)))

(defn- ->corrections
  "Returns what the model read from the statement that the user can
  correct"
  [{:as ingestion :statement-ingestion/keys [statement]}]
  (-> statement
      (select-keys [:period-start
                    :period-end
                    :opening-balance
                    :closing-balance])
      (update-in-if [:period-start] dates/unserialize-local-date)
      (update-in-if [:period-end] dates/unserialize-local-date)
      (assoc :account (statement-account ingestion))))

(defn- ->ingestion-corrections
  "Returns the corrections, given as the parts of the statement the user can
  correct, in the form the server takes them"
  [{:keys [account] :as corrections}]
  (cond-> #:statement-ingestion{:statement
                                (-> corrections
                                    (dissoc :account)
                                    (update-in-if [:period-start] dates/serialize-local-date)
                                    (update-in-if [:period-end] dates/serialize-local-date))}
    account (assoc :statement-ingestion/account (select-keys account [:id]))))

(defn- correct
  "Sends the corrections to the statement (see si/update) and follows the
  reconciliation of it"
  [reading corrections]
  (let [{::keys [callbacks] :as ingestion} @reading]
    (reset! reading (select-keys ingestion [:id]))
    (si/update ingestion
               corrections
               ; keep the corrections, so the user can try again
               :on-failure (fn [_] (reset! reading ingestion))
               :on-success (fn [updated]
                             (reset! reading updated)
                             (await-ingestion reading updated callbacks)))))

(defn- prompt-actions
  [reading caption title on-click]
  [:div.d-flex
   [button {:html {:class "btn-primary"
                   :type :button
                   :title title
                   :on-click on-click}
            :icon :check
            :caption caption}]
   [button {:html {:class "btn-secondary ms-2"
                   :type :button
                   :title "Click here to set the statement aside."
                   :on-click #(reset! reading nil)}
            :icon :x
            :caption "Cancel"}]])

(defn- correction-form
  "Shows what the model read from a statement that couldn't be reconciled,
  for the user to correct. It isn't a form element of its own, as it's
  shown within the reconciliation form."
  [reading]
  (let [corrections (r/atom (->corrections @reading))]
    (fn []
      [:div.card.mt-2
       [:div.card-body
        [:div.alert.alert-danger (:statement-ingestion/error @reading)]
        [:p "Correct what was read from the statement, and try again."]
        [forms/typeahead-field
         corrections
         [:account]
         {:search-fn (fn [input callback]
                       (callback (find-by-path input @accounts)))
          :caption-fn path-of
          :value-fn identity
          :find-fn (fn [current callback] (callback current))}]
        [forms/date-field corrections [:period-start] {}]
        [forms/date-field corrections [:period-end] {}]
        [forms/decimal-field corrections [:opening-balance] {:fraction-digits 2}]
        [forms/decimal-field corrections [:closing-balance] {:fraction-digits 2}]
        [prompt-actions
         reading
         "Try Again"
         "Click here to reconcile the statement as corrected."
         #(correct reading (->ingestion-corrections @corrections))]]])))

(defn- retry-prompt
  "Shows why the statement couldn't be reconciled, for the user to correct
  it in the form in which it's shown, e.g., the reconciliation form, from
  which corrections-fn returns the corrections"
  [reading corrections-fn]
  [:div.mt-2
   [:div.alert.alert-danger (:statement-ingestion/error @reading)]
   [:p "Correct what was read from the statement below, and try again."]
   [prompt-actions
    reading
    "Try Again"
    "Click here to reconcile the statement as corrected."
    #(correct reading (->ingestion-corrections (corrections-fn)))]])

(defn- replace-prompt
  "Asks the user whether the reconciliation in progress should be replaced
  by the statement"
  [reading]
  [:div.mt-2
   [:div.alert.alert-warning (:statement-ingestion/error @reading)]
   [:p "Replace the reconciliation in progress with the statement?"]
   [prompt-actions
    reading
    "Replace"
    "Click here to replace the reconciliation in progress with the statement."
    #(correct reading #:statement-ingestion{:replace? true})]])

(defn reading-indicator
  "Shows that the statement is being read, or, if it couldn't be
  reconciled, what was read from it, for the user to correct. When
  corrections-fn is given, the statement is corrected in the form that
  shows it (see retry-prompt) rather than in a form of its own."
  [reading & {:keys [corrections-fn]}]
  (when-let [{:statement-ingestion/keys [status]} @reading]
    (case status
      :failed (if corrections-fn
                [retry-prompt reading corrections-fn]
                [correction-form reading])
      :conflict [replace-prompt reading]
      [:div.d-flex.align-items-center.mt-2
       [:div.spinner-border.spinner-border-sm.me-2 {:role :status}]
       [:span "Reading the statement..."]])))
