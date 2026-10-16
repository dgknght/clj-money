(ns clj-money.entities.propagation-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.core.async :as a]
            [java-time.api :as t]
            [integrant.core :as ig]
            [dgknght.app-lib.test-assertions]
            [clj-money.otel :as otel]
            [clj-money.entities :as entities]
            [clj-money.entities.ref]
            [clj-money.db.ref]
            [clj-money.entities.propagation :as prop]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-entity
                                            find-account]]
            [clj-money.test-helpers :refer [dbtest
                                            await-propagation]]))

(defn- take!!
  [ch]
  (first (a/alts!! [ch (a/timeout 1000)])))

(deftest messages-are-delivered-in-publish-order-across-topics
  (let [publisher (ig/init-key ::entities/publisher {})
        ch (entities/sub publisher [:account ::done])]
    (try
      (entities/signal publisher :other {:n 0})
      (a/put! (:ch publisher) [nil #:account{:name "Checking"}])
      (entities/signal publisher ::done {:n 1})
      (is (= [nil #:account{:name "Checking"}]
             (take!! ch))
          "The change is delivered first")
      (is (= 1 (:n (take!! ch)))
          "The signal is delivered after the change")
      (finally
        (entities/unsub publisher ch)
        (ig/halt-key! ::entities/publisher publisher)))))

(deftest halting-the-publisher-closes-its-subscriptions
  (let [publisher (ig/init-key ::entities/publisher {})
        ch (entities/sub publisher [:account])]
    (ig/halt-key! ::entities/publisher publisher)
    (is (nil? (take!! ch))
        "The subscribed channel is closed")))

(defn- paycheck
  [entity description]
  #:transaction{:transaction-date (t/local-date 2015 1 1)
                :entity entity
                :description description
                :debit-account (find-account "Checking")
                :credit-account (find-account "Salary")
                :quantity 1000M})

(dbtest nothing-is-published-when-publishing-is-suppressed
  (with-context basic-context
    (entities/put (paycheck (find-entity "Personal") "Paycheck")
                  :publisher nil)
    (await-propagation)
    (is (= 0M (:account/quantity (entities/find (find-account "Checking"))))
        "The transaction is not propagated")))

(dbtest a-failed-propagation-does-not-stop-the-subscription
  (with-context basic-context
    (let [entity (find-entity "Personal")
          propagate prop/propagate]
      (with-redefs [prop/propagate (fn [[_ after :as change]]
                                     (if (= "Fail" (:transaction/description after))
                                       (throw (AssertionError. "Induced failure"))
                                       (propagate change)))]
        (entities/put (paycheck entity "Fail"))
        (entities/put (paycheck entity "Paycheck"))
        (await-propagation))
      ; propagating the second transaction re-indexes every item on or
      ; after its date, including the failed transaction's item
      (is (= 2000M (:account/quantity (entities/find (find-account "Checking"))))
          "Changes after the failed one are propagated"))))

(dbtest await-propagation-times-out
  (with-context basic-context
    (let [publisher (ig/init-key ::entities/publisher {})
          subscription (ig/init-key ::prop/subscription {:publisher publisher})
          released (promise)
          propagate prop/propagate]
      (try
        (with-redefs [prop/propagate (fn [change]
                                       (deref released 5000 nil)
                                       (propagate change))]
          (entities/put (paycheck (find-entity "Personal") "Paycheck")
                        :publisher publisher)
          (is (false? (prop/await-propagation subscription {:timeout-ms 50}))
              "False is returned when propagation doesn't finish in time")
          (deliver released true)
          (is (true? (prop/await-propagation subscription))
              "True is returned when propagation finishes"))
        (finally
          (deliver released true)
          (ig/halt-key! ::prop/subscription subscription)
          (ig/halt-key! ::entities/publisher publisher))))))

(defn- record-metrics
  "Calls f with otel/record redefined, and returns the recorded
  [value attributes] pairs, keyed by instrument name"
  [f]
  (await-propagation) ; don't record the propagation of the context
  (let [recorded (atom {})
        instruments {@#'prop/duration :duration
                     @#'prop/lag :lag}]
    (with-redefs [otel/record (fn [instrument value attributes]
                                (swap! recorded
                                       update
                                       (instruments instrument instrument)
                                       (fnil conj [])
                                       [value attributes]))]
      (f))
    @recorded))

(dbtest record-the-duration-and-lag-of-a-propagation
  (with-context basic-context
    (let [{[[duration duration-attrs]] :duration
           [[lag lag-attrs]] :lag}
          (record-metrics
            (fn []
              (entities/put (paycheck (find-entity "Personal") "Paycheck"))
              (await-propagation)))]
      (is (and (number? duration) (not (neg? duration)))
          "The duration is recorded")
      (is (= {"entity.type" :transaction} duration-attrs)
          "The duration is recorded with the entity type")
      (is (and (number? lag) (<= duration lag))
          "The lag is recorded and includes the duration")
      (is (= {"entity.type" :transaction} lag-attrs)
          "The lag is recorded with the entity type"))))

(dbtest record-the-error-type-of-a-failed-propagation
  (with-context basic-context
    (let [{[[_ duration-attrs]] :duration
           [[_ lag-attrs]] :lag}
          (record-metrics
            (fn []
              (with-redefs [prop/propagate (fn [_]
                                             (throw (AssertionError. "Induced failure")))]
                (entities/put (paycheck (find-entity "Personal") "Fail"))
                (await-propagation))))]
      (is (= {"entity.type" :transaction
              "error.type" "java.lang.AssertionError"}
             duration-attrs)
          "The duration is recorded with the error type")
      (is (= "java.lang.AssertionError" (lag-attrs "error.type"))
          "The lag is recorded with the error type"))))

(dbtest observe-the-size-of-the-propagation-queues
  (with-context basic-context
    (let [observe (atom nil)
          released (promise)
          propagate prop/propagate
          publisher (ig/init-key ::entities/publisher {})
          subscription (with-redefs [otel/gauge (fn [_name f & _]
                                                  (reset! observe f)
                                                  ::gauge)
                                     otel/close-gauge (fn [_])]
                         (ig/init-key ::prop/subscription {:publisher publisher}))
          sizes #(into {} (map (fn [[v attrs]] [(attrs "queue") v])) (@observe))]
      (try
        (is (= {"publisher" 0 "subscription" 0} (sizes))
            "Both queues are empty before anything is published")
        (with-redefs [prop/propagate (fn [change]
                                       (deref released 5000 nil)
                                       (propagate change))]
          (doseq [description ["One" "Two" "Three"]]
            (entities/put (paycheck (find-entity "Personal") description)
                          :publisher publisher))
          ; one change is being propagated and the other two wait,
          ; though briefly one may be in flight between the queues
          (is (= 2 (loop [n 0]
                     (let [waiting (reduce + (vals (sizes)))]
                       (if (or (= 2 waiting) (< 100 n))
                         waiting
                         (do (Thread/sleep 10)
                             (recur (inc n)))))))
              "Waiting changes are counted")
          (deliver released true)
          (prop/await-propagation subscription))
        (is (= {"publisher" 0 "subscription" 0} (sizes))
            "Both queues are empty once the changes are propagated")
        (finally
          (deliver released true)
          (with-redefs [otel/close-gauge (fn [_])]
            (ig/halt-key! ::prop/subscription subscription))
          (ig/halt-key! ::entities/publisher publisher))))))
