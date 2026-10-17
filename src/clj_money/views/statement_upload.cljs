(ns clj-money.views.statement-upload
  "Uploads a statement and follows the read of it, for the statements page
  and the reconciliation form"
  (:require [cljs.pprint :refer [pprint]]
            [dgknght.app-lib.notifications :as notify]
            [clj-money.icons :refer [icon-with-text]]
            [clj-money.state :refer [accounts-by-id]]
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
  [reading ingestion on-complete]
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
                      (do
                        (notify/dangerf "Unable to read the statement: %s" error)
                        (reset! reading nil))

                      (:processing :pending)
                      (do
                        (reset! reading updated)
                        (await-ingestion reading updated on-complete))

                      (do
                        (notify/dangerf "Unrecognized ingestion status: %s" (pr-str status))
                        (reset! reading nil))))))))
    poll-interval))

(defn read-statement
  "Uploads the statement and follows the read of it, keeping the
  statement-ingestion in the reading atom while it's in progress. When the
  account is given, the statement is reconciled against it. When the read
  is finished, on-complete is called with the account the statement was
  reconciled against."
  [file reading & {:keys [account on-complete]}]
  (reset! reading {:id ::uploading})
  (si/create file
             :account account
             :on-failure (fn [_] (reset! reading nil))
             :on-success (fn [ingestion]
                           (reset! reading ingestion)
                           (await-ingestion reading ingestion on-complete))))

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

(defn reading-indicator
  [reading]
  (when @reading
    [:div.d-flex.align-items-center.mt-2
     [:div.spinner-border.spinner-border-sm.me-2 {:role :status}]
     [:span "Reading the statement..."]]))
