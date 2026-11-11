(ns clj-money.reconciliations-test
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer [deftest is]])
            [clj-money.decimal :as d]
            [clj-money.dates :as dates]
            [clj-money.reconciliations :as reconciliations]))

(deftest a-liability-account-requires-payment
  (is (reconciliations/requires-payment? #:account{:type :liability})))

(deftest an-asset-account-does-not-require-payment
  (is (not (reconciliations/requires-payment? #:account{:type :asset}))))

(deftest build-a-payment-template-for-a-reconciled-account
  (let [account #:account{:id :credit-card
                          :name "Credit Card"
                          :type :liability}]
    (is (= #:transaction{:other-account account
                         :quantity (d/d 100)}
           (reconciliations/->payment account (d/d 100))))))

(defn- recon
  [id date-str]
  {:id id
   :reconciliation/end-of-period (dates/unserialize-local-date date-str)})

(defn- simplify
  [{:keys [reconciliations next-on-or-before fill-date]}]
  (cond-> {:ids (mapv :id reconciliations)}
    next-on-or-before (assoc :next-on-or-before
                             (dates/serialize-local-date next-on-or-before))
    fill-date (assoc :fill-date (dates/serialize-local-date fill-date))))

(deftest a-short-page-is-the-last-one
  (is (= {:ids [1 2]}
         (simplify (reconciliations/split-page [(recon 1 "2026-03-31")
                                                (recon 2 "2026-02-28")]
                                               3)))
      "All of the reconciliations are shown, with nothing more to read"))

(deftest a-full-page-leaves-its-last-date-for-the-next-one
  (is (= {:ids [1 2]
          :next-on-or-before "2026-01-31"}
         (simplify (reconciliations/split-page [(recon 1 "2026-03-31")
                                                (recon 2 "2026-02-28")
                                                (recon 3 "2026-01-31")]
                                               3)))
      "The reconciliations ending on the last date are read with the next page"))

(deftest a-full-page-on-one-date-is-filled
  (is (= {:ids []
          :fill-date "2026-01-31"}
         (simplify (reconciliations/split-page [(recon 1 "2026-01-31")
                                                (recon 2 "2026-01-31")]
                                               2)))
      "All of the reconciliations ending on the date are read"))
