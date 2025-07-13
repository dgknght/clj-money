(ns clj-money.models.lots-test
  (:require [clojure.test :refer [is]]
            [java-time.api :as t]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.test_assertions]
            [clj-money.util :as util]
            [clj-money.models :as models]
            [clj-money.models.ref]
            [clj-money.db.ref]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [with-context
                                            find-account
                                            find-commodity
                                            find-lot]]
            [clj-money.model-helpers :refer [assert-invalid
                                             assert-created]]
            [clj-money.test-helpers :refer [dbtest]]))

(def ^:private lot-context
  [(factory :user {:user/email "john@doe.com"})
   #:entity{:name "Personal"
            :user "john@doe.com"}
   #:commodity{:name "US Dollar"
               :entity "Personal"
               :symbol "USD"
               :type :currency}
   #:commodity{:name "Apple"
               :entity "Personal"
               :symbol "AAPL"
               :exchange :nasdaq
               :type :stock}
   #:account{:name "IRA"
             :entity "Personal"
             :type :asset
             :commodity "USD"}
   #:account{:name "Dining"
             :entity "Personal"
             :type :expense
             :commodity "USD"}
   #:account{:name "Checking"
             :entity "Personal"
             :type :asset
             :commodity "USD"}])

(defn- attributes []
  #:lot{:commodity (find-commodity "AAPL")
        :account (find-account "IRA")
        :purchase-date (t/local-date 2017 3 2)
        :purchase-price 10M
        :shares-purchased 100M})

(dbtest create-a-lot
  (with-context lot-context
    (assert-created (attributes) :refs [:lot/commodity :lot/account])))

(dbtest commodity-is-required
  (with-context lot-context
    (assert-invalid (dissoc (attributes) :lot/commodity)
                    {:lot/commodity ["Commodity is required"]})))

(dbtest account-is-required
  (with-context lot-context
    (assert-invalid (dissoc (attributes) :lot/account)
                    {:lot/account ["Account is required"]})))

(dbtest purchase-price-is-required
  (with-context lot-context
    (assert-invalid (dissoc (attributes) :lot/purchase-price)
                    {:lot/purchase-price ["Purchase price is required"]})))

(dbtest account-id-must-reference-an-asset-account
  (with-context lot-context
    (assert-invalid (assoc (attributes) :lot/account (find-account "Dining"))
                    {:lot/account ["Account must be an asset"]})))

(dbtest purchase-date-is-required
  (with-context lot-context
    (assert-invalid (dissoc (attributes) :lot/purchase-date)
                    {:lot/purchase-date ["Purchase date is required"]})))

(dbtest purchase-date-must-be-a-date
  (with-context lot-context
    (assert-invalid (assoc (attributes) :lot/purchase-date "not-a-date")
                    {:lot/purchase-date ["Purchase date is invalid"]})))

(dbtest shares-purchased-is-required
  (with-context lot-context
    (assert-invalid (dissoc (attributes) :lot/shares-purchased)
                    {:lot/shares-purchased ["Shares purchased is required"]})))

(def ^:private existing-lot-context
  (conj lot-context
        #:lot{:account "IRA"
              :commodity "AAPL"
              :purchase-price 10M
              :shares-purchased 100M
              :shares-owned 100M
              :purchase-date (t/local-date 2016 3 2)}))

(dbtest update-a-lot
  (with-context existing-lot-context
    (let [lot (find-lot ["IRA" "AAPL"])]
      (is (comparable? #:lot{:shares-owned 70M}
                       (-> lot
                           (assoc :lot/shares-owned 70M)
                           models/put))
          "The return value has the specified attributes")
      (is (comparable? #:lot{:shares-owned 70M}
                       (models/find lot))
          "The retrieved value has the specified attributes"))))

(dbtest search-lots-by-account
  (with-context existing-lot-context
    (let [ira (find-account "IRA")]
      (is (seq-of-maps-like? [#:lot{:commodity (util/->model-ref (find-commodity "AAPL"))
                                    :account (util/->model-ref ira)
                                    :purchase-date (t/local-date 2016 3 2)
                                    :purchase-price 10M
                                    :shares-purchased 100M
                                    :shares-owned 100M}]
                             (models/select {:lot/account (find-account "IRA")}))))))

; Test unrealized-gains with:
;   Date that precedes some purchases
;   Date that precedes some sales
