(ns clj-money.ingestion.statement-reconciliation
  "Reconciles an account from a statement read by a model"
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log]
            [java-time.api :as t]
            [dgknght.app-lib.validation :as v]
            [clj-money.util :as util :refer [id=]]
            [clj-money.images :as images]
            [clj-money.entities :as entities]
            [clj-money.entities.reconciliations :as recs]
            [clj-money.accounts :refer [trading?]]
            [clj-money.trading :as trading]
            [clj-money.ingestion :as ing]
            [clj-money.ingestion.statements :as stmts]
            [clj-money.ingestion.investment-statements :as inv]))

(defn- error-message
  [e]
  (if-let [errors (::v/errors (ex-data e))]
    (str "The statement could not be reconciled. "
         (->> (vals errors)
              flatten
              (filter string?)
              (str/join " ")))
    (ex-message e)))

(defn- resolve-account
  [{:keys [account]} given accounts-by-path]
  (or given
      (accounts-by-path account)
      (throw (ex-info "Unable to tell which account the statement is for."
                      {:account account}))))

(defn- ensure-after-last-reconciliation
  "Throws if the statement doesn't come after the last completed
  reconciliation, which is checked before anything is saved, so that nothing
  needs to be undone"
  [account {:keys [period-end]}]
  (when-let [last-completed (stmts/last-completed-reconciliation account)]
    (when-not (t/after? period-end (:reconciliation/end-of-period last-completed))
      (throw (ex-info (format "The statement ends on %s, which is not after the last reconciliation, on %s."
                              period-end
                              (:reconciliation/end-of-period last-completed))
                      {:period-end period-end})))))

(defn- ensure-no-conflict
  "Throws if the account has a reconciliation in progress that doesn't end on
  the statement's closing date with its closing balance, unless it's to be
  replaced, so that the user can confirm that it should be"
  [account {:keys [period-end closing-balance]} replace?]
  (when-not replace?
    (when-let [{:reconciliation/keys [end-of-period balance]}
               (stmts/reconciliation-in-progress account)]
      (when-not (and (= period-end end-of-period)
                     (zero? (compare closing-balance balance)))
        (throw (ex-info (format "%s has a reconciliation in progress, ending on %s with a balance of %s, which doesn't match the statement, ending on %s with a balance of %s."
                                (:account/name account)
                                end-of-period
                                balance
                                period-end
                                closing-balance)
                        {::conflict true}))))))

(defn- release-reconciliation-items
  "Removes the items from the reconciliations in progress of the account
  and, for a trading account, its commodity accounts, so that the
  statement's items take their place. This is done before any transaction
  is created: propagating a new transaction re-puts the items after it in
  the account, as they were read, which would put an item removed from the
  reconciliation in the meantime back in it."
  [account]
  (doseq [a (cons account
                  (when (trading? account)
                    (vals (inv/commodity-accounts account))))]
    (stmts/release-items a)))

(defn- warnings
  [account {:keys [opening-balance]}]
  (let [previous (or (recs/previous-balance account) 0M)]
    (cond-> []
      (not (zero? (compare opening-balance previous)))
      (conj (format "The statement's opening balance, %s, doesn't match the balance of the last reconciliation, %s."
                    opening-balance
                    previous)))))

(defn- attach-statement
  "Attaches the statement to each of the transactions. This is done once
  everything else is saved: updating the transaction's count of attachments
  in the background could otherwise restore a transaction deleted because
  the reconciliation failed."
  [trxs image]
  (when (seq trxs)
    (entities/put-many (map #(hash-map :attachment/transaction (util/->entity-ref %)
                                       :attachment/image image
                                       :attachment/caption "Statement")
                            trxs))))

(defn- create-transactions
  "Saves a transaction for each statement line that matched nothing already
  recorded, and returns the items they add to the account"
  [lines account {:keys [entity accounts-by-path created]}]
  (when (seq lines)
    (let [saved (->> lines
                     (map #(stmts/make-trx % account entity accounts-by-path))
                     entities/put-many
                     (filter (util/entity-type? :transaction)))]
      (swap! created into saved)
      (->> saved
           (mapcat :transaction/items)
           (filter #(id= account (:transaction-item/account %)))))))

(defn- create-trades
  "Records each trade that matched nothing already recorded, and returns
  the items of its transactions"
  [trades account {:keys [accounts-by-path created]}]
  (let [trxs (into []
                   (mapcat (fn [{:keys [type] :as line}]
                             (let [trade (inv/make-trade line account accounts-by-path)
                                   trxs (:trade/transactions (if (= "sell" type)
                                                               (trading/sell trade)
                                                               (trading/buy trade)))]
                               (swap! created into trxs)
                               trxs)))
                   trades)]
    (mapcat :transaction/items trxs)))

(defn- reconcile-cash-account
  "Matches the lines to the items in the account, records the lines that
  match none, and saves the reconciliation. Returns any warnings."
  [statement account {:keys [image] :as opts}]
  (let [{:keys [matched unmatched]} (stmts/match-items
                                      (:transactions statement)
                                      (stmts/candidate-items account statement))
        new-items (create-transactions unmatched account opts)]
    (entities/put (stmts/make-reconciliation account
                                             statement
                                             (concat (map second matched)
                                                     new-items)
                                             :image image))
    []))

(defn- reconcile-trading-account
  "Matches the lines to the items in the trading account and its commodity
  accounts, records the lines that match none, and saves a reconciliation
  for the cash and for each commodity. Returns any warnings."
  [statement account {:keys [entity image] :as opts}]
  (let [lines (inv/resolve-lines (:transactions statement) entity)
        {:keys [items
                unmatched-trades
                unmatched-cash]} (inv/match-lines lines
                                                  account
                                                  (inv/commodity-accounts account)
                                                  statement)
        new-items (concat (create-trades unmatched-trades account opts)
                          (create-transactions unmatched-cash account opts))
        ; a purchase may have added a commodity account
        commodity-accounts (inv/commodity-accounts account)
        reconciled? (comp (set (cons (:id account)
                                     (map :id (vals commodity-accounts))))
                          :id
                          :transaction-item/account)
        {:keys [reconciliations warnings]} (inv/make-reconciliations
                                             account
                                             statement
                                             (filter reconciled?
                                                     (concat items new-items))
                                             commodity-accounts
                                             :image image)]
    (entities/put-many reconciliations)
    (concat (:warnings lines) warnings)))

(defn- fail
  "Saves the ingestion as failed, with the error, or, if the statement
  conflicts with a reconciliation in progress, as awaiting the user's
  confirmation that it should be replaced"
  [ingestion e]
  (entities/put (assoc ingestion
                       :statement-ingestion/status (if (::conflict (ex-data e))
                                                     :conflict
                                                     :failed)
                       :statement-ingestion/error (error-message e))))

(defn reconcile-statement
  "Matches the lines of the statement read for the ingestion to the
  account's transactions, creates transactions for the lines that match
  none, and saves the account's reconciliation, unfinished, for the user to
  complete. For a trading account, the trades are matched to the commodity
  accounts, and each of them is reconciled too. Returns the ingestion,
  complete, or failed with an error. A failed ingestion keeps the
  statement, so that the user can correct it and try again.

  If the account has a reconciliation in progress that matches the
  statement, the lines are added to it. If it doesn't match, nothing is
  changed, and the ingestion is left awaiting the user's confirmation
  (:conflict) that it should be replaced, which is done when replace? is
  true. A replaced reconciliation's items are removed before anything else
  is saved, and aren't restored if the statement can't be reconciled."
  [ingestion & {:keys [replace?]}]
  (let [{:statement-ingestion/keys [entity image account statement] :as ingestion}
        (entities/put (assoc ingestion :statement-ingestion/status :processing))
        entity (entities/find entity)
        given (some-> account entities/find)
        created (atom [])]
    (try
      (let [by-path (stmts/accounts-by-path entity)
            normalized (stmts/normalize statement)
            account (resolve-account normalized given by-path)
            _ (ensure-after-last-reconciliation account normalized)
            _ (ensure-no-conflict account normalized replace?)
            _ (when replace? (release-reconciliation-items account))
            account-warnings ((if (trading? account)
                                reconcile-trading-account
                                reconcile-cash-account)
                              normalized
                              account
                              {:entity entity
                               :image image
                               :accounts-by-path by-path
                               :created created})]
        (attach-statement @created image)
        (entities/put (assoc ingestion
                             :statement-ingestion/status :complete
                             :statement-ingestion/account (util/->entity-ref account)
                             :statement-ingestion/error nil
                             :statement-ingestion/warnings (vec (concat (warnings account normalized)
                                                                        account-warnings)))))
      (catch Exception e
        (if (::conflict (ex-data e))
          (log/infof "[statement-ingestion] statement %s awaits confirmation: %s"
                     (:id ingestion)
                     (ex-message e))
          (log/errorf e "[statement-ingestion] unable to reconcile statement %s" (:id ingestion)))
        ; the transactions were created for the reconciliation, so they go
        ; if it couldn't be saved. A trade is undone, restoring its lots,
        ; the latest first.
        (doseq [trx (reverse @created)]
          (some-> (entities/find trx) trading/delete-transaction))
        (fail ingestion e)))))

(defn- read-statement
  "Returns the ingestion with the statement uploaded with it, as read by the
  reader, or, if it can't be read, the ingestion saved as failed"
  [{:statement-ingestion/keys [entity image account] :as ingestion} reader]
  (try
    (let [entity (entities/find entity)]
      (assoc ingestion
             :statement-ingestion/statement
             (ing/read-statement reader
                                 (images/get (:image/uuid (entities/find image)))
                                 entity
                                 {:account-options (stmts/account-options
                                                     entity
                                                     :account (some-> account entities/find))})))
    (catch Exception e
      (log/errorf e "[statement-ingestion] unable to read statement %s" (:id ingestion))
      (fail ingestion e))))

(defn reconcile
  "Reads the statement uploaded with the ingestion and reconciles the
  account against it (see reconcile-statement). Returns the ingestion,
  complete, or failed with an error."
  [ingestion reader]
  (let [ingestion (read-statement (entities/put (assoc ingestion
                                                       :statement-ingestion/status
                                                       :processing))
                                  reader)]
    (if (= :failed (:statement-ingestion/status ingestion))
      ingestion
      (reconcile-statement ingestion))))
