(ns clj-money.views.statement-upload-test
  (:require [cljs.test :refer [deftest is async]]
            [dgknght.app-lib.notifications :as notify]
            [clj-money.cached-accounts :as cached-accts]
            [clj-money.api.statement-ingestions :as si]
            [clj-money.views.statement-upload :as statements]))

(def ^:private complete-ingestion
  #:statement-ingestion{:status :complete
                        :account {:id 101}
                        :warnings ["The opening balance doesn't match"]
                        :statement {:period-start "2026-08-20"
                                    :period-end "2026-09-18"}})

(defn- read-with
  "Reads a statement with the API stubbed to return the given updates in
  turn, calling done with what was captured once on-complete is called, or
  the read is abandoned. The stubs are set, rather than redefined, because
  the read continues after with-redefs would have restored them."
  [updates done]
  (let [reading (atom nil)
        captured (atom {:warnings []
                        :errors []
                        :pushed []})
        remaining (atom updates)
        originals [statements/poll-interval si/create si/get notify/warn
                   notify/dangerf cached-accts/fetch-accounts
                   cached-accts/push-transaction-date!]
        restore (fn []
                  (let [[pi c g w d f p] originals]
                    (set! statements/poll-interval pi)
                    (set! si/create c)
                    (set! si/get g)
                    (set! notify/warn w)
                    (set! notify/dangerf d)
                    (set! cached-accts/fetch-accounts f)
                    (set! cached-accts/push-transaction-date! p)))
        finish (fn []
                 (restore)
                 (done (assoc @captured :reading @reading)))]
    (set! statements/poll-interval 1)
    (set! si/create (fn [_file & {:keys [on-success]}]
                      (on-success {:id 201})))
    (set! si/get (fn [_ingestion & {:keys [on-success]}]
                   (let [[update & more] @remaining]
                     (reset! remaining more)
                     (on-success (assoc update :id 201))
                     (when (= :failed (:statement-ingestion/status update))
                       (finish)))))
    (set! notify/warn #(swap! captured update :warnings conj %))
    (set! notify/dangerf (fn [msg & args]
                           (swap! captured update :errors conj (apply str msg args))))
    (set! cached-accts/fetch-accounts (fn [& _]))
    (set! cached-accts/push-transaction-date! (fn [account date]
                                                (swap! captured update :pushed conj (str date))
                                                account))
    (statements/read-statement #js {:name "statement.pdf"}
                               reading
                               :on-complete (fn [account]
                                              (swap! captured assoc :account account)
                                              (finish)))))

(deftest a-completed-read-reports-the-account
  (async done
         (read-with [#:statement-ingestion{:status :processing}
                     complete-ingestion]
                    (fn [{:keys [account warnings pushed reading]}]
                      (is (= {:id 101} account)
                          "The account the statement was reconciled against is given")
                      (is (= ["The opening balance doesn't match"] warnings)
                          "The warnings are shown")
                      (is (= 2 (count pushed))
                          "The account's transaction date range is extended to the statement period")
                      (is (nil? reading)
                          "The read is no longer followed")
                      (done)))))

(deftest a-failed-read-is-reported
  (async done
         (read-with [#:statement-ingestion{:status :failed
                                           :error "The model is unavailable"}]
                    (fn [{:keys [account errors reading]}]
                      (is (nil? account))
                      (is (= 1 (count errors)))
                      (is (nil? reading))
                      (done)))))
