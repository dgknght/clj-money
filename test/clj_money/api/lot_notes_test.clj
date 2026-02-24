(ns clj-money.api.lot-notes-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [java-time.api :as t]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.test]
            [clj-money.json]
            [clj-money.util :as util]
            [clj-money.factories.user-factory]
            [clj-money.api.test-helper :refer [parse-body
                                               request]]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-user
                                            find-account
                                            find-commodity
                                            find-lot-note]]
            [clj-money.entities :as entities]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.web.server :refer [app]]))

(use-fixtures :each reset-db)

(def ^:private base-context
  (conj basic-context
        #:commodity{:name "Apple"
                    :entity "Personal"
                    :symbol "AAPL"
                    :exchange :nasdaq
                    :type :stock}
        #:commodity{:name "Microsoft"
                    :entity "Personal"
                    :symbol "MSFT"
                    :exchange :nasdaq
                    :type :stock}
        #:account{:name "IRA"
                  :entity "Personal"
                  :type :asset
                  :commodity "USD"
                  :system-tags #{:trading}}
        #:account{:name "AAPL"
                  :entity "Personal"
                  :type :asset
                  :parent "IRA"
                  :commodity "AAPL"
                  :system-tags #{:tradable}}
        #:account{:name "MSFT"
                  :entity "Personal"
                  :type :asset
                  :parent "IRA"
                  :commodity "MSFT"
                  :system-tags #{:tradable}}
        #:lot{:account "IRA"
              :commodity "AAPL"
              :purchase-price 150M
              :shares-purchased 10M
              :shares-owned 10M
              :purchase-date (t/local-date 2020 1 15)}
        #:lot{:account "IRA"
              :commodity "AAPL"
              :purchase-price 80M
              :shares-purchased 10M
              :shares-owned 10M
              :purchase-date (t/local-date 2020 6 15)}
        #:lot{:account "IRA"
              :commodity "MSFT"
              :purchase-price 50M
              :shares-purchased 10M
              :shares-owned 10M
              :purchase-date (t/local-date 2020 1 15)}))

(def ^:private list-context
  (conj base-context
        #:lot-note{:lots [["IRA" "AAPL" (t/local-date 2020 1 15)]
                          ["IRA" "AAPL" (t/local-date 2020 6 15)]]
                   :transaction-date (t/local-date 2021 1 15)
                   :memo "2-for-1 stock split"}))

(defn- list-lot-notes
  [email]
  (let [account (find-account "AAPL")]
    (-> (request :get (path :api
                            :accounts
                            (:id account)
                            :lot-notes)
                 :user (find-user email))
        app
        parse-body)))

(defn- assert-successful-list
  [{:as response :keys [parsed-body]}]
  (is (http-success? response))
  (is (seq-of-maps-like?
        [{:lot-note/transaction-date (t/local-date 2021 1 15)
          :lot-note/memo "2-for-1 stock split"}]
        parsed-body)
      "The response body contains the lot note data"))

(defn- assert-blocked-list
  [{:as response :keys [parsed-body]}]
  (is (http-success? response))
  (is (empty? parsed-body) "The body is empty"))

(deftest a-user-can-list-lot-notes-for-his-lot
  (with-context list-context
    (assert-successful-list (list-lot-notes "john@doe.com"))))

(deftest a-user-cannot-list-lot-notes-for-anothers-lot
  (with-context list-context
    (assert-blocked-list (list-lot-notes "jane@doe.com"))))

(defn- create-lot-note
  [email]
  (-> (request :post (path :api
                           :commodities
                           (:id (find-commodity "AAPL"))
                           :lot-notes)
               :body #:lot-note{:transaction-date (t/local-date 2022 3 1)
                                :memo "3-for-1 stock split"}
               :user (find-user email))
      app
      parse-body))

(defn- assert-successful-create
  [{:keys [parsed-body] :as response}]
  (is (http-created? response))
  (is (:id parsed-body) "An ID is assigned to the new record")
  (is (comparable? {:lot-note/memo "3-for-1 stock split"}
                   (entities/find (:id parsed-body)))
      "The created note can be retrieved"))

(defn- assert-blocked-create
  [response]
  (is (http-not-found? response))
  (is (empty? (entities/select
                (util/entity-type
                  {:lot/commodity (find-commodity "AAPL")}
                  :lot-note)))))

(deftest a-user-can-create-a-lot-note-for-his-lot
  (with-context base-context
    (assert-successful-create (create-lot-note "john@doe.com"))))

(deftest a-user-cannot-create-a-lot-note-for-anothers-lot
  (with-context base-context
    (assert-blocked-create (create-lot-note "jane@doe.com"))))

(defn- delete-lot-note
  [email]
  (let [note (find-lot-note [(t/local-date 2021 1 15) "2-for-1 stock split"])
        response (-> (request :delete (path :api :lot-notes (:id note))
                              :user (find-user email))
                     app)]
    [response (entities/find note)]))

(defn- assert-successful-delete
  [[response retrieved]]
  (is (http-success? response))
  (is (nil? retrieved) "The note cannot be retrieved after delete"))

(defn- assert-blocked-delete
  [[response retrieved]]
  (is (http-not-found? response))
  (is retrieved "The note can still be retrieved after a blocked delete"))

(deftest a-user-can-delete-a-lot-note-in-his-entity
  (with-context list-context
    (assert-successful-delete (delete-lot-note "john@doe.com"))))

(deftest a-user-cannot-delete-a-lot-note-in-anothers-entity
  (with-context list-context
    (assert-blocked-delete (delete-lot-note "jane@doe.com"))))
