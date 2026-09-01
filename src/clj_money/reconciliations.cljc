(ns clj-money.reconciliations
  (:require [clj-money.decimal :as d]))

(defn requires-payment?
  "Returns true if reconciling the account can leave a balance that needs to
  be paid off (i.e. a liability, like a credit card), as opposed to an asset
  account (like checking or savings), which is never paid off."
  [account]
  (= :liability (:account/type account)))

(defn ->payment
  "Given the account that was just reconciled and the reconciled statement
  balance, returns a payment transaction template used to pre-populate the
  payment modal shown after reconciling a liability account.

  Does not set :transaction/account (the payment account) since it may not
  yet be configured on the reconciled account - the caller resolves that
  from the account's :account/payment-account, if present, and otherwise
  lets the user choose one.

  The quantity is always non-negative - it's the plain payment amount
  moving from the payment account (credited) to the reconciled account
  (debited, reducing the balance owed). The caller negates it relative to
  the payment account (via clj-money.transactions/unaccountify) before
  saving the transaction."
  [account balance]
  #:transaction{:other-account account
                :quantity (d/abs balance)})
