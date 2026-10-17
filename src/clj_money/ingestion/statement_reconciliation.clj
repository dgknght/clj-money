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
            [clj-money.ingestion :as ing]
            [clj-money.ingestion.statements :as stmts]))

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
  (when-let [last-completed (entities/find-by
                              #:reconciliation{:account (util/->entity-ref account)
                                               :status :completed}
                              {:sort [[:reconciliation/end-of-period :desc]]})]
    (when-not (t/after? period-end (:reconciliation/end-of-period last-completed))
      (throw (ex-info (format "The statement ends on %s, which is not after the last reconciliation, on %s."
                              period-end
                              (:reconciliation/end-of-period last-completed))
                      {:period-end period-end})))))

(defn- warnings
  [account {:keys [opening-balance]}]
  (let [previous (or (recs/previous-balance account) 0M)]
    (cond-> []
      (not (zero? (compare opening-balance previous)))
      (conj (format "The statement's opening balance, %s, doesn't match the balance of the last reconciliation, %s."
                    opening-balance
                    previous)))))

(defn- create-transactions
  "Saves a transaction for each statement line that matched nothing already
  recorded, each with the statement attached, and returns the items they
  add to the account"
  [lines account entity image accounts-by-path]
  (when (seq lines)
    (let [trxs (mapv #(assoc (stmts/make-trx % account entity accounts-by-path)
                             :id (util/temp-id))
                     lines)
          atts (map #(hash-map :attachment/transaction %
                               :attachment/image image
                               :attachment/caption "Statement")
                    trxs)]
      (->> (entities/put-many (concat trxs atts))
           (filter (util/entity-type? :transaction))
           (mapcat :transaction/items)
           (filter #(id= account (:transaction-item/account %)))))))

(defn reconcile
  "Reads the statement uploaded with the ingestion, matches its lines to the
  account's transactions, creates transactions for the lines that match
  none, and saves the account's reconciliation, unfinished, for the user to
  complete. Returns the ingestion, complete, or failed with an error."
  [ingestion reader]
  (let [{:statement-ingestion/keys [entity image account] :as ingestion}
        (entities/put (assoc ingestion :statement-ingestion/status :processing))
        entity (entities/find entity)
        given (some-> account entities/find)
        created (atom nil)]
    (try
      (let [by-path (stmts/accounts-by-path entity)
            raw (ing/read-statement reader
                                    (images/get (:image/uuid (entities/find image)))
                                    entity
                                    {:account-options (stmts/account-options entity
                                                                             :account given)})
            statement (stmts/normalize raw)
            account (resolve-account statement given by-path)
            _ (ensure-after-last-reconciliation account statement)
            {:keys [matched unmatched]} (stmts/match-items
                                          (:transactions statement)
                                          (stmts/candidate-items account statement))
            new-items (reset! created (create-transactions unmatched
                                                           account
                                                           entity
                                                           image
                                                           by-path))
            _ (entities/put (stmts/make-reconciliation account
                                                       statement
                                                       (concat (map second matched)
                                                               new-items)
                                                       :image image))]
        (entities/put (assoc ingestion
                             :statement-ingestion/status :complete
                             :statement-ingestion/account (util/->entity-ref account)
                             :statement-ingestion/statement raw
                             :statement-ingestion/warnings (warnings account statement))))
      (catch Exception e
        (log/errorf e "[statement-ingestion] unable to read statement %s" (:id ingestion))
        ; the transactions were created for the reconciliation, so they go
        ; if it couldn't be saved
        (when-let [trx-ids (->> @created
                                (map (comp :id :transaction-item/transaction))
                                distinct
                                seq)]
          (entities/delete-many (entities/find-many trx-ids)))
        (entities/put (assoc ingestion
                             :statement-ingestion/status :failed
                             :statement-ingestion/error (error-message e)))))))
