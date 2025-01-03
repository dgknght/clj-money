(ns clj-money.api.prices-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [ring.mock.request :as req]
            [clj-factory.core :refer [factory]]
            [lambdaisland.uri :refer [map->query-string]]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test :refer [parse-json-body]]
            [dgknght.app-lib.test-assertions]
            [java-time.api :as t]
            [clj-money.util :as util]
            [clj-money.factories.user-factory]
            [clj-money.dates :as dates :refer [with-fixed-time]]
            [clj-money.api.test-helper :refer [add-auth]]
            [clj-money.test-context :refer [with-context
                                            find-user
                                            find-price
                                            find-commodity]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.prices.yahoo :as yahoo]
            [clj-money.models :as models]
            [clj-money.models.prices :as prices]
            [clj-money.web.server :refer [app]]))

(use-fixtures :each reset-db)

(def ^:private context
  [(factory :user {:user/email "john@doe.com"})
   (factory :user {:user/email "jane@doe.com"})
   #:entity{:name "Personal"
            :user "john@doe.com"}
   #:commodity{:name "Apple, Inc"
               :entity "Personal"
               :symbol "AAPL"
               :exchange :nasdaq
               :type :stock}
   #:commodity{:name "Some Fund"
               :entity "Personal"
               :symbol "FND"
               :exchange :nasdaq
               :type :fund}])

(defn- create-a-price
  [email]
  (with-context context
    (let [commodity (find-commodity "AAPL")]
      [(-> (req/request :post (path :api
                                    :commodities
                                    (:id commodity)
                                    :prices))
           (req/json-body #:price{:price 12.34
                                  :trade-date "2016-03-02"})
           (add-auth (find-user email))
           app
           parse-json-body)
       (models/select #:price{:commodity commodity
                              :trade-date (t/local-date 2016 3 2)})])))

(defn- assert-successful-create
  [[{:as response :keys [json-body]} retrieved]]
  (is (http-success? response))
  (is (comparable? #:price{:price 12.34
                           :trade-date "2016-03-02"}
                   json-body)
      "The created price is returned in the response")
  (is (seq-of-maps-like? [#:price{:price 12.34M
                                  :trade-date (t/local-date 2016 3 2)}]
                         retrieved)
      "The price is created in the database"))

(defn- assert-blocked-create
  [[response _ retrieved]]
  (is (http-not-found? response))
  (is (empty? retrieved) "The database record is not created"))

(deftest a-user-can-create-a-price-for-a-commodity-in-his-entity
  (assert-successful-create (create-a-price "john@doe.com")))

(deftest a-user-cannot-create-a-price-for-a-commodity-in-anothers-entity
  (assert-blocked-create (create-a-price "jane@doe.com")))

(def ^:private list-context
  (conj context
        #:price{:commodity "AAPL"
                :trade-date (t/local-date 2016 2 27)
                :price 10M}
        #:price{:commodity "AAPL"
                :trade-date (t/local-date 2016 3 2)
                :price 11M}
        #:price{:commodity "FND"
                :trade-date (t/local-date 2016 2 27)
                :price 10.01M}
        #:price{:commodity "FND"
                :trade-date (t/local-date 2016 3 2)
                :price 9.99M}))

(defn- get-a-list-by-commodity
  [email]
  (with-context list-context
    (let [commodity (find-commodity "AAPL")]
      (-> (req/request :get (str (path :api :prices)
                                 "?"
                                 (map->query-string
                                   {:trade-date ["2016-01-01" "2016-12-31"]
                                    :commodity-id (:id commodity)})))
          (add-auth (find-user email))
          app
          parse-json-body))))

(defn- assert-successful-list
  [{:as response :keys [json-body]}]
  (is (http-success? response))
  (is (seq-of-maps-like? [#:price{:trade-date "2016-03-02"
                                  :price 11.0}
                          #:price{:trade-date "2016-02-27"
                                  :price 10.0}]
                         json-body)))

(defn- assert-blocked-list
  [{:as response :keys [json-body]}]
  (is (http-success? response))
  (is (empty? json-body) "The body is empty"))

(deftest a-user-can-get-a-list-of-prices-from-his-entity
  (assert-successful-list (get-a-list-by-commodity "john@doe.com")))

(deftest a-user-cannot-get-a-list-of-prices-from-anothers-entity
  (assert-blocked-list (get-a-list-by-commodity "jane@doe.com")))

(defn- update-a-price
  [email]
  (with-context list-context
    (let [price (find-price ["AAPL" (t/local-date 2016 2 27)])]
      [(-> (req/request :patch (path :api
                                     :prices
                                     (dates/serialize-local-date (:price/trade-date price))
                                     (:id price)))
           (req/json-body #:price{:price 9.99})
           (add-auth (find-user email))
           app
           parse-json-body)
       (models/find price)])))

(defn- assert-successful-update
  [[{:as response :keys [json-body]} retrieved]]
  (is (http-success? response))
  (is (comparable? #:price{:trade-date "2016-02-27"
                           :price 9.99}
                   json-body)
      "The response contains the updated price")
  (is (comparable? #:price{:trade-date (t/local-date 2016 2 27)
                           :price 9.99M}
                   retrieved)
      "The database record is updated"))

(defn- assert-blocked-update
  [[response retrieved]]
  (is (http-not-found? response))
  (is (comparable? #:price{:trade-date (t/local-date 2016 2 27)
                           :price 10M}
                   retrieved)
      "The database record is not updated"))

(deftest a-user-can-update-a-price-for-a-commodity-is-his-entity
  (assert-successful-update (update-a-price "john@doe.com")))

(deftest a-user-cannot-update-a-price-for-a-commodity-in-anothers-entity
  (assert-blocked-update (update-a-price "jane@doe.com")))

(defn- delete-a-price
  [email]
  (with-context list-context
    (let [price (find-price ["AAPL" (t/local-date 2016 2 27)])]
      [(-> (req/request :delete (path :api
                                      :prices
                                      (dates/serialize-local-date (:price/trade-date price))
                                      (:id price)))
           (add-auth (find-user email))
           app)
       (models/find price)])))

(defn- assert-successful-delete
  [[response retrieved]]
  (is (http-success? response))
  (is (nil? retrieved) "The price cannot be retrieved after delete."))

(defn- assert-blocked-delete
  [[response retrieved]]
  (is (http-not-found? response))
  (is retrieved "The price can still be retrieved after blocked delete."))

(deftest a-user-can-delete-a-price-in-his-entity
  (assert-successful-delete (delete-a-price "john@doe.com")))

(deftest a-user-cannot-delete-a-price-in-anothers-entity
  (assert-blocked-delete (delete-a-price "jane@doe.com")))

(def ^:private fetch-context
  [#:user{:first-name "John"
          :last-name "Doe"
          :email "john@doe.com"
          :password "please01"}
   #:entity{:user "john@doe.com"
            :name "Personal"}
   #:commodity{:entity "Personal"
               :name "Apple, Inc."
               :symbol "AAPL"
               :exchange :nasdaq
               :type :stock}
   #:commodity{:entity "Personal"
               :name "Microsoft, Inc."
               :symbol "MSFT"
               :exchange :nasdaq
               :type :stock}])

(defn- fetch-some-prices
  [username]
  (let [appl (find-commodity "AAPL")
        msft (find-commodity "MSFT")]
    (with-redefs [yahoo/get-quotes (fn [symbols]
                                     (map (fn [s]
                                            {:symbol s
                                             :fullExchangeName "NasdaqGS"
                                             :regularMarketPrice ({"AAPL" 10.01M
                                                                   "MSFT" 5.01M}
                                                                  s)
                                             :regularMarketTime (t/local-date 2015 3 2)})
                                          symbols))]
      (with-fixed-time "2015-03-02T12:00:00Z"
        (-> (req/request :get (str (path :api
                                         :prices
                                         :fetch)
                                   "?commodity-id="
                                   (:id appl)
                                   "&commodity-id="
                                   (:id msft)))
            (add-auth
              (when username
                (find-user username)))
            app
            parse-json-body)))))

(defn- assert-successful-fetch
  [response]
  (is (http-success? response))
  (is (seq-of-maps-like? [#:price{:trade-date "2015-03-02"
                                  :price 10.01
                                  :commodity (util/->model-ref (find-commodity "AAPL")) }
                          #:price{:trade-date "2015-03-02"
                                  :price 5.01
                                  :commodity (util/->model-ref (find-commodity "MSFT"))}]
                         (:json-body response))
      "The prices are returned in the response")
  (is (seq-of-maps-like? [#:price{:trade-date (t/local-date 2015 3 2)
                                  :price 10.01M
                                  :commodity (util/->model-ref (find-commodity "AAPL"))}
                          #:price{:trade-date (t/local-date 2015 3 2)
                                  :price 5.01M
                                  :commodity (util/->model-ref (find-commodity "MSFT"))}]
                         (models/select #:price{:trade-date (t/local-date 2015 3 2)}))
      "The prices are written to the database"))

(deftest a-user-can-fetch-a-current-commodity-prices
  (with-context fetch-context
    (assert-successful-fetch (fetch-some-prices "john@doe.com"))))

(deftest an-unauthenticated-user-cannot-fetch-commodity-prices
  (with-context fetch-context
    (is (http-unauthorized? (fetch-some-prices nil)))))

(deftest prices-are-only-fetched-once-per-day
  (with-context fetch-context
    (let [user (find-user "john@doe.com")
          appl (find-commodity "AAPL")
          msft (find-commodity "MSFT")
          calls (atom [])
          make-req #(-> (req/request :get (str (path :api
                                                     :prices
                                                     :fetch)
                                               "?commodity-id="
                                               (:id appl)
                                               "&commodity-id="
                                               (:id msft)))
                        (add-auth user)
                        app
                        parse-json-body)]
      (with-redefs [yahoo/get-quotes (fn [symbols]
                                       (swap! calls conj symbols)
                                       (map (fn [s]
                                              {:symbol s
                                               :regularMarketPrice 10.01M
                                               :regularMarketTime (t/local-date 2015 3 2)
                                               :fullExchangeName "NasdaqGS"})
                                            symbols))]
        (t/with-clock (t/fixed-clock (t/instant (t/formatter :iso-instant) "2015-03-02T12:00:00Z"))
          (make-req)
          (make-req))
        (let [[c :as cs ] @calls]
          (is (= 1 (count cs))
            "The external service is called exactly one time")
          (is (= ["AAPL" "MSFT"]
                 c)
              "The external service is called with the specified commodity symbols"))))))
