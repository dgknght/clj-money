(ns clj-money.api.prices-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.pprint :refer [pprint]]
            [clj-factory.core :refer [factory]]
            [lambdaisland.uri :refer [map->query-string uri]]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.test]
            [java-time.api :as t]
            [clj-money.json]
            [clj-money.util :as util]
            [clj-money.factories.user-factory]
            [clj-money.dates :as dates :refer [with-fixed-time]]
            [clj-money.api.test-helper :refer [parse-body
                                               request
                                               ->json-entity-ref]]
            [clj-money.test-context :refer [with-context
                                            find-user
                                            find-price
                                            find-commodity]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.prices.alpha-vantage :as alpha]
            [clj-money.entities :as entities]
            [clj-money.entity-helpers :refer [jsonify]]
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
  [email & {:keys [content-type body]
            :or {content-type "application/edn"
                 body #:price{:value 12.34M
                              :trade-date (t/local-date 2016 3 2)}}}]
  (let [commodity (find-commodity "AAPL")]
    [(-> (request :post (path :api
                              :commodities
                              (:id commodity)
                              :prices)
                  :content-type content-type
                  :body body
                  :user (find-user email))
         app
         parse-body)
     (entities/select #:price{:commodity commodity
                              :trade-date (t/local-date 2016 3 2)})]))

(defn- assert-successful-create
  [[{:as response :keys [parsed-body]} retrieved]
   & {:keys [expected expected-response]
      :or {expected #:price{:value 12.34M
                            :trade-date (t/local-date 2016 3 2)}}}]
  (is (http-success? response))
  (is (comparable? (or expected-response expected) parsed-body)
      "The created price is returned in the response")
  (is (seq-of-maps-like? [expected] retrieved)
      "The price is created in the database"))

(defn- assert-blocked-create
  [[response _ retrieved]]
  (is (http-not-found? response))
  (is (empty? retrieved) "The database record is not created"))

(deftest a-user-can-create-a-price-for-a-commodity-in-his-entity
  (with-context context
    (assert-successful-create (create-a-price "john@doe.com"))))

(deftest a-user-can-create-a-price-for-a-commodity-in-his-entity-with-json
  (with-context context
    (assert-successful-create
      (create-a-price "john@doe.com"
                      :content-type "application/json"
                      :body {:value 12.34
                             :tradeDate "2016-03-02"
                             :_type "price"})
      :expected-response {:value {:d 12.34}
                          :tradeDate "2016-03-02"
                          :_type "price"})))

(deftest a-user-cannot-create-a-price-for-a-commodity-in-anothers-entity
  (with-context context
    (assert-blocked-create (create-a-price "jane@doe.com"))))

(def ^:private list-context
  (conj context
        #:price{:commodity "AAPL"
                :trade-date (t/local-date 2016 2 27)
                :value 10M}
        #:price{:commodity "AAPL"
                :trade-date (t/local-date 2016 3 2)
                :value 11M}
        #:price{:commodity "FND"
                :trade-date (t/local-date 2016 2 27)
                :value 10.01M}
        #:price{:commodity "FND"
                :trade-date (t/local-date 2016 3 2)
                :value 9.99M}))

(defn- get-a-list-by-commodity
  [email & {:keys [content-type]
            :or {content-type "application/edn"}}]
  (let [commodity (find-commodity "AAPL")
        url (-> (uri (path :api
                           :commodities
                           (:id commodity)
                           :prices))
                (assoc :query
                       (map->query-string
                         {:trade-date-on-or-after "2016-01-01"
                          :trade-date-before "2017-01-01"}))
                str)]
    (-> (request :get url
                 :content-type content-type
                 :user (find-user email))
        app
        parse-body)))

(defn- assert-successful-list
  [{:as response :keys [parsed-body]}
   & {:keys [expected]
      :or {expected [#:price{:trade-date (t/local-date 2016 3 2)
                             :value 11.0M}
                     #:price{:trade-date (t/local-date 2016 2 27)
                             :value 10.0M}]}}]
  (is (http-success? response))
  (is (seq-of-maps-like? expected parsed-body)))

(defn- assert-blocked-list
  [{:as response :keys [parsed-body]}]
  (is (http-success? response))
  (is (empty? parsed-body) "The body is empty"))

(deftest a-user-can-get-a-list-of-prices-from-his-entity
  (with-context list-context
    (testing "default format"
      (assert-successful-list (get-a-list-by-commodity "john@doe.com")))
    (testing "json format"
      (assert-successful-list
        (get-a-list-by-commodity "john@doe.com" :content-type "application/json")
        :expected [{:tradeDate "2016-03-02"
                    :value {:d 11.0}
                    :_type "price"}
                   {:tradeDate "2016-02-27"
                    :value {:d 10.0}
                    :_type "price"}]))))

(deftest a-user-cannot-get-a-list-of-prices-from-anothers-entity
  (with-context list-context
    (assert-blocked-list (get-a-list-by-commodity "jane@doe.com"))))

(defn- update-a-price
  [email & {:keys [content-type body]
            :or {content-type "application/edn"
                 body #:price{:value 9.99M}}}]
  (let [price (find-price ["AAPL" (t/local-date 2016 2 27)])]
    [(-> (request :patch (path :api
                               :prices
                               (dates/serialize-local-date (:price/trade-date price))
                               (:id price))
                  :content-type content-type
                  :body body
                  :user (find-user email))
         app
         parse-body)
     (entities/find price)]))

(defn- assert-successful-update
  [[{:as response :keys [parsed-body]} retrieved]
   & {:keys [expected expected-response]
      :or {expected #:price{:trade-date (t/local-date 2016 2 27)
                            :value 9.99M}}}]
  (is (http-success? response))
  (is (comparable? (or expected-response expected) parsed-body)
      "The response contains the updated price")
  (is (comparable? expected retrieved)
      "The database record is updated"))

(defn- assert-blocked-update
  [[response retrieved]]
  (is (http-not-found? response))
  (is (comparable? #:price{:trade-date (t/local-date 2016 2 27)
                           :value 10M}
                   retrieved)
      "The database record is not updated"))

(deftest a-user-can-update-a-price-for-a-commodity-is-his-entity
  (with-context list-context
    (assert-successful-update (update-a-price "john@doe.com"))))

(deftest a-user-can-update-a-price-for-a-commodity-is-his-entity-with-json
  (with-context list-context
    (assert-successful-update
      (update-a-price "john@doe.com"
                      :content-type "application/json"
                      :body {:value 9.99
                             :_type "price"})
      :expected-response {:tradeDate "2016-02-27"
                          :value {:d 9.99}
                          :_type "price"})))

(deftest a-user-cannot-update-a-price-for-a-commodity-in-anothers-entity
  (with-context list-context
    (assert-blocked-update (update-a-price "jane@doe.com"))))

(defn- delete-a-price
  [email & {:keys [content-type]
            :or {content-type "application/edn"}}]
  (let [price (find-price ["AAPL" (t/local-date 2016 2 27)])]
    [(-> (request :delete (path :api
                                :prices
                                (dates/serialize-local-date (:price/trade-date price))
                                (:id price))
                  :content-type content-type
                  :user (find-user email))
         app)
     (entities/find price)]))

(defn- assert-successful-delete
  [[response retrieved]]
  (is (http-success? response))
  (is (nil? retrieved) "The price cannot be retrieved after delete."))

(defn- assert-blocked-delete
  [[response retrieved]]
  (is (http-not-found? response))
  (is retrieved "The price can still be retrieved after blocked delete."))

(deftest a-user-can-delete-a-price-in-his-entity
  (with-context list-context
    (assert-successful-delete (delete-a-price "john@doe.com"))))

(deftest a-user-cannot-delete-a-price-in-anothers-entity
  (with-context list-context
    (assert-blocked-delete (delete-a-price "jane@doe.com"))))

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
  [username & {:keys [content-type]
               :or {content-type "application/edn"}}]
  (let [appl (find-commodity "AAPL")
        msft (find-commodity "MSFT")]
    (with-redefs [alpha/get-quote (fn [symbol]
                                    {:commodity/symbol symbol
                                     :commodity/exchange :nasdaq
                                     :price/trade-date (t/local-date 2015 3 2)
                                     :price/value ({"AAPL" 10.01M
                                                    "MSFT" 5.01M}
                                                   symbol)})]
      (with-fixed-time "2015-03-02T12:00:00Z"
        (-> (request :get (str (path :api
                                     :prices
                                     :fetch)
                               "?commodity-id="
                               (:id appl)
                               "&commodity-id="
                               (:id msft))
                     :content-type content-type
                     :user (when username
                             (find-user username)))
            app
            parse-body)))))

(defn- assert-successful-fetch
  [{:as response :keys [parsed-body]}
   & {:keys [expected expected-response]
      :or {expected [#:price{:trade-date (t/local-date 2015 3 2)
                             :value 10.01M
                             :commodity (util/->entity-ref (find-commodity "AAPL"))}
                     #:price{:trade-date (t/local-date 2015 3 2)
                             :value 5.01M
                             :commodity (util/->entity-ref (find-commodity "MSFT"))}]}}]
  (is (http-success? response))
  (is (= (set
           (or expected-response
               (map #(update-in %
                                [:price/commodity]
                                ->json-entity-ref)
                    expected)))
         (->> parsed-body
              (map #(dissoc % :id))
              set))
      "The prices are returned in the response")
  (is (= (set expected)
         (->> (entities/select #:price{:trade-date (t/local-date 2015 3 2)})
              (map #(dissoc %
                            :id
                            :price/created-at
                            :price/updated-at))
              set))
      "The prices are written to the database"))

(deftest a-user-can-fetch-current-commodity-prices
  (with-context fetch-context
    (assert-successful-fetch (fetch-some-prices "john@doe.com"))))

(deftest a-user-can-fetch-current-commodity-prices-with-json
  (with-context fetch-context
    (let [[aapl msft] (map (comp util/->entity-ref
                                 find-commodity)
                           ["AAPL" "MSFT"])]
      (assert-successful-fetch
        (fetch-some-prices "john@doe.com" :content-type "application/json")
        :expected-response (jsonify
                             [{:tradeDate "2015-03-02"
                               :value {:d 10.01}
                               :commodity aapl
                               :_type "price"}
                              {:tradeDate "2015-03-02"
                               :value {:d 5.01}
                               :commodity msft
                               :_type "price"}])))))

(deftest an-unauthenticated-user-cannot-fetch-commodity-prices
  (with-context fetch-context
    (is (http-unauthorized? (fetch-some-prices nil)))))

(deftest prices-are-only-fetched-once-per-day
  (with-context fetch-context
    (let [user (find-user "john@doe.com")
          appl (find-commodity "AAPL")
          msft (find-commodity "MSFT")
          calls (atom [])
          make-req #(-> (request :get (str (path :api
                                                   :prices
                                                   :fetch)
                                             "?commodity-id="
                                             (:id appl)
                                             "&commodity-id="
                                             (:id msft))
                                 :content-type "application/edn"
                                 :user user)
                        app
                        parse-body)]
      (with-redefs [alpha/get-quote (fn [symbol]
                                      (swap! calls conj symbol)
                                      {:commodity/symbol symbol
                                       :commodity/exchange :nasdaq
                                       :price/value 10.01M
                                       :price/trade-date (t/local-date 2015 3 2)})]
        (t/with-clock (t/fixed-clock (t/instant (t/formatter :iso-instant) "2015-03-02T12:00:00Z"))
          (make-req)
          (make-req))
        (is (= {"MSFT" 1
                "AAPL" 1}
               (frequencies @calls))
            "The external service is called exactly one time for each symbol")))))
