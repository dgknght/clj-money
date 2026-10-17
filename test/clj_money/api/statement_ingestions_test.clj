(ns clj-money.api.statement-ingestions-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [ring.mock.request :as req]
            [cheshire.core :as json]
            [cheshire.parse :as json-parse]
            [camel-snake-kebab.core :refer [->kebab-case-keyword]]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.test]
            [clj-money.entities.ref]
            [clj-money.db.ref]
            [clj-money.util :as util]
            [clj-money.ingestion :as ing]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.api.test-helper :refer [add-auth
                                               parse-body
                                               request
                                               build-multipart-request]]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-user
                                            find-entity
                                            find-account
                                            find-transaction]]
            [clj-money.entities :as entities]
            [clj-money.web.test-handler :refer [build-app]]))

(use-fixtures :each reset-db)

(defn- read-fixture
  "Returns the expected response for the statement, as the reader returns it"
  [statement-name]
  (binding [json-parse/*use-bigdecimals?* true]
    (json/parse-string (slurp (io/resource (format "fixtures/statements/%s.json"
                                                   statement-name)))
                       ->kebab-case-keyword)))

(defn- reader
  "Returns a reader that returns the result of calling f with the opts"
  [f]
  (reify
    ing/StatementReader
    (read-statement [_ _source _entity opts] (f opts))
    ing/Closeable
    (close [_])))

(def ^:private checking-reader
  (reader (fn [_] (read-fixture "checking"))))

(def ^:private failing-reader
  (reader (fn [_] (throw (ex-info "The model is unavailable" {})))))

(def ^:private statement-context
  (conj basic-context
        #:account{:name "Credit Card"
                  :entity "Personal"
                  :type :liability}
        #:account{:name "Cash"
                  :entity "Personal"
                  :type :asset}
        #:account{:name "Interest"
                  :entity "Personal"
                  :type :income}
        #:account{:name "Gifts"
                  :entity "Personal"
                  :type :expense}
        #:account{:name "Utilities"
                  :entity "Personal"
                  :type :expense}
        #:account{:name "Household"
                  :entity "Personal"
                  :type :expense}
        #:transaction{:transaction-date (t/local-date 2026 8 1)
                      :entity "Personal"
                      :description "Opening balance"
                      :quantity 3512.40M
                      :debit-account "Checking"
                      :credit-account "Opening Balances"}
        #:transaction{:transaction-date (t/local-date 2026 8 25)
                      :entity "Personal"
                      :description "Riverside Apartments"
                      :quantity 1250M
                      :debit-account "Rent"
                      :credit-account "Checking"}
        #:transaction{:transaction-date (t/local-date 2026 9 1)
                      :entity "Personal"
                      :description "Paycheck"
                      :quantity 2450M
                      :debit-account "Checking"
                      :credit-account "Salary"}
        ; recorded on the day of the purchase, three days before it posted
        #:transaction{:transaction-date (t/local-date 2026 9 5)
                      :entity "Personal"
                      :description "Corner Hardware"
                      :quantity 42.17M
                      :debit-account "Household"
                      :credit-account "Checking"}
        #:transaction{:transaction-date (t/local-date 2026 9 3)
                      :entity "Personal"
                      :description "Jordan Lee"
                      :quantity 75M
                      :debit-account "Gifts"
                      :credit-account "Checking"}
        #:reconciliation{:account "Checking"
                         :end-of-period (t/local-date 2026 8 19)
                         :balance 3512.40M
                         :status :completed
                         :items [[(t/local-date 2026 8 1) 3512.40M]]}))

(defn- await-completion
  "Waits for the background read of the statement-ingestion to finish,
  returning the statement-ingestion as it was last retrieved."
  [{:keys [id]}]
  (loop [n 0]
    (let [ingestion (entities/find id)]
      (if (or (#{:complete :failed} (:statement-ingestion/status ingestion))
              (<= 100 n))
        ingestion
        (do (Thread/sleep 100)
            (recur (inc n)))))))

(defn- create-ingestion
  [email & {:keys [reader account]
            :or {reader checking-reader}}]
  (let [entity (find-entity "Personal")
        file (io/file (io/resource "fixtures/statements/checking.pdf"))
        response (-> (req/request :post (path :api
                                              :entities
                                              (:id entity)
                                              :statement-ingestions))
                     (merge (build-multipart-request
                              (cond-> {:statement {:file file
                                                   :content-type "application/pdf"}}
                                account (assoc :account-id (str (:id account))))))
                     (add-auth (find-user email))
                     (req/header "Accept" "application/edn")
                     ((build-app {:statement-reader reader}))
                     parse-body)]
    [response
     (some-> response
             :parsed-body
             await-completion)]))

(defn- working-reconciliation
  [ingestion]
  (some-> ingestion
          :statement-ingestion/account
          (#(entities/find-by #:reconciliation{:account %
                                              :status :new}))))

(defn- reconciled-items
  [ingestion]
  (when-let [recon (working-reconciliation ingestion)]
    (->> (entities/select
           (util/entity-type
             {:transaction-item/reconciliation (util/->entity-ref recon)}
             :transaction-item)
           {:select-also [:transaction/transaction-date
                          :transaction/description]})
         (map (juxt :transaction/transaction-date
                    :transaction-item/quantity))
         set)))

(deftest a-user-can-reconcile-a-statement-in-his-entity
  (with-context statement-context
    (let [[{:as response :keys [parsed-body]} retrieved] (create-ingestion "john@doe.com")
          checking (find-account "Checking")]
      (is (http-created? response))
      (is (comparable? #:statement-ingestion{:status :pending}
                       parsed-body)
          "The response is the new statement-ingestion, waiting to be read")
      (is (comparable? #:statement-ingestion{:status :complete
                                             :account (util/->entity-ref checking)}
                       retrieved)
          "The account identified by the model is recorded")
      (is (empty? (:statement-ingestion/warnings retrieved))
          "There are no warnings when the opening balance matches")
      (is (comparable? #:reconciliation{:status :new
                                        :end-of-period (t/local-date 2026 9 18)
                                        :balance 5936.31M}
                       (working-reconciliation retrieved))
          "An unfinished reconciliation is created for the statement")
      (is (= (:statement-ingestion/image retrieved)
             (:reconciliation/statement (working-reconciliation retrieved)))
          "The reconciliation refers to the statement's image")
      (is (= #{[(t/local-date 2026 8 22) 200M]
               [(t/local-date 2026 8 25) 1250M]
               [(t/local-date 2026 8 28) 87.43M]
               [(t/local-date 2026 9 1) 2450M]
               [(t/local-date 2026 9 3) 75M]
               [(t/local-date 2026 9 4) 134.22M]
               [(t/local-date 2026 9 5) 42.17M]
               [(t/local-date 2026 9 9) 612.35M]
               [(t/local-date 2026 9 15) 2450M]
               [(t/local-date 2026 9 16) 75M]
               [(t/local-date 2026 9 18) 0.08M]}
             (reconciled-items retrieved))
          "Every statement line is in the reconciliation, using the existing items where they match")
      (is (nil? (:transaction/source
                  (entities/find (find-transaction [(t/local-date 2026 9 1) "Paycheck"]))))
          "A matching transaction is left as it was")
      (let [trx (entities/find-by #:transaction{:transaction-date (t/local-date 2026 9 16)
                                                :entity (find-entity "Personal")})]
        (is (comparable? #:transaction{:description "Zelle Payment To Jordan Lee"
                                       :source :ingestion
                                       :review-status :pending}
                         trx)
            "A transaction awaiting review is created for an unmatched line")
        (is (seq-of-maps-like? [#:attachment{:image (:statement-ingestion/image retrieved)
                                             :caption "Statement"}]
                               (entities/select #:attachment{:transaction trx}))
            "The statement is attached to the new transaction"))
      (is (= 7 (count (entities/select #:transaction{:entity (find-entity "Personal")
                                                     :source :ingestion})))
          "A transaction is created for each unmatched line"))))

(deftest new-transactions-use-the-category-chosen-by-the-model
  (with-context statement-context
    (create-ingestion "john@doe.com")
    (let [trx (entities/find-by #:transaction{:transaction-date (t/local-date 2026 9 9)
                                              :entity (find-entity "Personal")})
          items (->> (:transaction/items trx)
                     (map (juxt (comp :id :transaction-item/account)
                                :transaction-item/action))
                     set)]
      (is (= #{[(:id (find-account "Checking")) :credit]
               [(:id (find-account "Credit Card")) :debit]}
             items)
          "The card payment comes out of checking and pays down the card"))))

(deftest a-user-can-reconcile-a-statement-for-a-given-account
  (with-context statement-context
    (let [opts (atom nil)
          checking (find-account "Checking")
          [_ retrieved] (create-ingestion "john@doe.com"
                                          :account checking
                                          :reader (reader (fn [o]
                                                            (reset! opts o)
                                                            (dissoc (read-fixture "checking")
                                                                    :account))))]
      (is (nil? (get-in @opts [:account-options :statement-accounts]))
          "The model isn't asked to identify the account")
      (is (comparable? #:statement-ingestion{:status :complete
                                             :account (util/->entity-ref checking)}
                       retrieved)
          "The statement is reconciled against the given account"))))

(def ^:private in-progress-context
  (conj statement-context
        #:reconciliation{:account "Checking"
                         :end-of-period (t/local-date 2026 9 1)
                         :balance 0M
                         :status :new
                         :items [[(t/local-date 2026 8 25) 1250M]]}))

(def ^:private later-reconciliation-context
  (conj statement-context
        #:reconciliation{:account "Checking"
                         :end-of-period (t/local-date 2026 9 30)
                         :balance 3512.40M
                         :status :completed
                         :items []}))

(def ^:private other-entity-account-context
  (conj statement-context
        #:account{:name "Jane's Checking"
                  :entity "Business"
                  :type :asset}))

(deftest statement-lines-are-added-to-a-reconciliation-in-progress
  (with-context in-progress-context
    (let [[_ retrieved] (create-ingestion "john@doe.com")
          recons (entities/select #:reconciliation{:account (find-account "Checking")
                                                   :status :new})]
      (is (= 1 (count recons))
          "No other reconciliation is created")
      (is (comparable? #:reconciliation{:end-of-period (t/local-date 2026 9 18)
                                        :balance 5936.31M}
                       (first recons))
          "The reconciliation takes the statement's closing date and balance")
      (is (= 11 (count (reconciled-items retrieved)))
          "The reconciliation contains each statement line"))))

(deftest a-mismatched-opening-balance-is-reported
  (with-context statement-context
    (let [[_ retrieved] (create-ingestion "john@doe.com"
                                          :reader (reader (fn [_]
                                                            (assoc (read-fixture "checking")
                                                                   :opening-balance 3000M))))]
      (is (= :complete (:statement-ingestion/status retrieved)))
      (is (= ["The statement's opening balance, 3000, doesn't match the balance of the last reconciliation, 3512.40."]
             (:statement-ingestion/warnings retrieved))))))

(deftest a-statement-before-the-last-reconciliation-is-rejected
  (with-context later-reconciliation-context
    (let [[_ retrieved] (create-ingestion "john@doe.com")]
      (is (comparable? #:statement-ingestion{:status :failed
                                             :error "The statement ends on 2026-09-18, which is not after the last reconciliation, on 2026-09-30."}
                       retrieved))
      (is (empty? (entities/select #:transaction{:entity (find-entity "Personal")
                                                 :source :ingestion}))
          "No transactions are created"))))

(deftest a-failed-read-is-recorded
  (with-context statement-context
    (let [[response retrieved] (create-ingestion "john@doe.com"
                                                 :reader failing-reader)]
      (is (http-created? response))
      (is (comparable? #:statement-ingestion{:status :failed
                                             :error "The model is unavailable"}
                       retrieved)
          "The error is recorded on the statement-ingestion"))))

(deftest a-user-cannot-create-a-statement-ingestion-in-anothers-entity
  (with-context statement-context
    (let [[response] (create-ingestion "jane@doe.com")]
      (is (http-not-found? response))
      (is (empty? (entities/select (util/entity-type {} :statement-ingestion)))
          "No statement-ingestion is created"))))

(deftest a-user-cannot-reconcile-a-statement-against-anothers-account
  (with-context other-entity-account-context
    (let [[response] (create-ingestion "john@doe.com"
                                       :account (find-account "Jane's Checking"))]
      (is (http-not-found? response))
      (is (empty? (entities/select (util/entity-type {} :statement-ingestion)))
          "No statement-ingestion is created"))))

(defn- get-ingestion
  [id email]
  (-> (request :get (path :api :statement-ingestions id)
               :user (find-user email))
      ((build-app {:statement-reader checking-reader}))
      parse-body))

(deftest a-user-can-get-a-statement-ingestion-in-his-entity
  (with-context statement-context
    (let [[_ {:keys [id]}] (create-ingestion "john@doe.com")
          {:as response :keys [parsed-body]} (get-ingestion id "john@doe.com")]
      (is (http-success? response))
      (is (comparable? #:statement-ingestion{:status :complete}
                       parsed-body)
          "The statement-ingestion is returned"))))

(deftest a-user-cannot-get-a-statement-ingestion-in-anothers-entity
  (with-context statement-context
    (let [[_ {:keys [id]}] (create-ingestion "john@doe.com")]
      (is (http-not-found? (get-ingestion id "jane@doe.com"))))))
