(ns clj-money.reconciliations
  (:require [clj-money.decimal :as d]
            [clj-money.dates :as dates]))

(defn- same-date?
  [d1 d2]
  ; compared as strings, because cljs-time dates aren't equal by value
  (= (dates/serialize-local-date d1)
     (dates/serialize-local-date d2)))

(defn split-page
  "Given a page of reconciliations, latest first, read with the limit,
  returns a map of the :reconciliations to show and how to read the rest,
  which are those ending on or before the date in :next-on-or-before.
  Those ending on the date of the last one in the page may not all be in it,
  so they're left for the next page, unless the whole page ends on that
  date, in which case :fill-date is that date: all of those ending on it
  are read, then the ones ending before it. Neither is given once all have
  been read."
  [recons limit]
  (if (< (count recons) limit)
    {:reconciliations recons}
    (let [last-date (:reconciliation/end-of-period (last recons))
          shown (take-while #(not (same-date? last-date
                                              (:reconciliation/end-of-period %)))
                            recons)]
      (if (seq shown)
        {:reconciliations shown
         :next-on-or-before last-date}
        {:reconciliations []
         :fill-date last-date}))))

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
