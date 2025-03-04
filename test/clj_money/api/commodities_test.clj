(ns clj-money.api.commodities-test
  (:require [clojure.test :refer [deftest use-fixtures is]]
            [ring.mock.request :as req]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [realize
                                            find-user
                                            find-entity
                                            find-commodity]]
            [clj-money.test-helpers :refer [reset-db
                                            edn-body
                                            parse-edn-body]]
            [clj-money.api.test-helper :refer [add-auth]]
            [clj-money.models.commodities :as coms]
            [clj-money.web.server :refer [app]]))

(use-fixtures :each reset-db)

(def ^:private context
  {:users (->> ["john@doe.com" "jane@doe.com"]
               (mapv #(factory :user {:email %})))
   :entities [{:name "Personal"
               :user-id "john@doe.com"}
              {:name "Business"
               :user-id "jane@doe.com"}]
   :commodities [{:name "US Dollar"
                  :symbol "USD"
                  :type :currency
                  :entity-id "Personal"}
                 {:name "Microsoft, Inc"
                  :symbol "MSFT"
                  :type :stock
                  :exchange :nasdaq
                  :entity-id "Personal"}]})

(defn- get-a-count-of-commodities
  [email]
  (let [ctx (realize context)
        user (find-user ctx email)
        entity (find-entity ctx "Personal")]
    (-> (req/request :get (path :api
                                :entities
                                (:id entity)
                                :commodities
                                :count))
        (add-auth user)
        app
        parse-edn-body)))

(defn- assert-successful-count
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (= {:count 2} edn-body) "The body contains the count"))

(defn- assert-blocked-count
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (= {:count 0} edn-body) "The body contains a count of zero"))

(deftest a-user-can-get-a-count-of-commodities-in-his-entity
  (assert-successful-count (get-a-count-of-commodities "john@doe.com")))

(deftest a-user-cannot-get-a-count-of-commodities-in-anothers-entity
  (assert-blocked-count (get-a-count-of-commodities "jane@doe.com")))

(defn- get-a-list-of-commodities
  [email]
  (let [ctx (realize context)
        user (find-user ctx email)
        entity (find-entity ctx "Personal")]
    (-> (req/request :get (path :api
                                :entities
                                (:id entity)
                                :commodities))
        (add-auth user)
        app
        parse-edn-body)))

(defn- assert-successful-list
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (seq-of-maps-like? [{:name "Microsoft, Inc"
                             :symbol "MSFT"
                             :type :stock
                             :exchange :nasdaq}
                            {:name "US Dollar"
                             :symbol "USD"
                             :type :currency}]
                           edn-body)
      "The body contains the list of commodities"))

(defn- assert-blocked-list
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (empty? edn-body) "The body is empty"))

(deftest a-user-can-get-a-list-of-commodities-in-his-entity
  (assert-successful-list (get-a-list-of-commodities "john@doe.com")))

(deftest a-user-cannot-get-a-list-of-commodities-in-anothers-entity
  (assert-blocked-list (get-a-list-of-commodities "jane@doe.com")))

(defn- get-a-commodity
  [email]
  (let [ctx (realize context)
        user (find-user ctx email)
        msft (find-commodity ctx "MSFT")]
    (-> (req/request :get (path :api
                                :commodities
                                (:id msft)))
        (add-auth user)
        app
        parse-edn-body)))

(defn- assert-successful-get
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (comparable? {:name "Microsoft, Inc"
                    :symbol "MSFT"
                    :type :stock
                    :exchange :nasdaq}
                   edn-body)
      "The specified commodity is returned in the response"))

(defn- assert-blocked-get
  [response]
  (is (http-not-found? response)))

(deftest a-user-can-get-a-commodity-in-his-entity
  (assert-successful-get (get-a-commodity "john@doe.com")))

(deftest a-user-cannot-get-a-commodity-in-aothers-entity
  (assert-blocked-get (get-a-commodity "jane@doe.com")))

(def ^:private commodity-attributes
  {:type :stock
   :name "Apple, Inc."
   :symbol "AAPL"
   :exchange :nasdaq
   :price-config {:enabled true}})

(defn- create-a-commodity
  [email]
  (let [ctx (realize context)
        user (find-user ctx email)
        entity (find-entity ctx "Personal")
        response (-> (req/request :post (path :api
                                              :entities
                                              (:id entity)
                                              :commodities))
                     (edn-body commodity-attributes)
                     (add-auth user)
                     app
                     parse-edn-body)
        retrieved (coms/search {:entity-id (:id entity)})]
    [response retrieved]))

(defn- assert-successful-create
  [[{:as response :keys [edn-body]} retrieved]]
  (is (http-created? response))
  (is (comparable? commodity-attributes
                   edn-body)
      "The newly created commodity is returned in the response")
  (is (seq-with-map-like? commodity-attributes
                          retrieved)
      "The new commodity can be retrieved from the database"))

(defn- assert-blocked-create
  [[response _ retrieved]]
  (is (http-not-found? response))
  (is (seq-with-no-map-like? (-> commodity-attributes
                                 (update-in [:type] keyword)
                                 (update-in [:exchange] keyword))
                             retrieved)
      "The commodity is not created"))

(deftest a-user-can-create-a-commodity-in-his-entity
  (assert-successful-create (create-a-commodity "john@doe.com")))

(deftest a-user-cannot-create-an-commodity-in-anothers-entity
  (assert-blocked-create (create-a-commodity "jane@doe.com")))

(deftest attempt-to-create-an-invalid-commodity
  (let [ctx (realize context)
        user (find-user ctx "john@doe.com")
        entity (find-entity ctx "Personal")
        response (-> (req/request :post (path :api
                                              :entities
                                              (:id entity)
                                              :commodities))
                     (edn-body (assoc commodity-attributes :exchange "notvalid"))
                     (add-auth user)
                     app
                     parse-edn-body)
        retrieved (coms/search {:entity-id (:id entity)})]
    (is (http-bad-request? response))
    (is (invalid? (:edn-body response) [:exchange] "Exchange must be amex, nasdaq, nyse, or otc"))
    (is (not-any? #(= "AAPL" (:symbol %)) retrieved) "The record is not created")))

(defn- update-a-commodity
  [email]
  (let [ctx (realize context)
        user (find-user ctx email)
        msft (find-commodity ctx "MSFT")
        response (-> (req/request :patch (path :api
                                               :commodities
                                               (:id msft)))
                     (edn-body (assoc msft :name "Microsoft, Ltd."))
                     (add-auth user)
                     app
                     parse-edn-body)
        retrieved (coms/find msft)]
    [response retrieved]))

(defn- assert-successful-update
  [[{:as response :keys [edn-body]} retrieved]]
  (is (http-success? response))
  (is (comparable? {:name "Microsoft, Ltd."}
                   edn-body)
      "The updated commodity is returned in the body")
  (is (comparable? {:name "Microsoft, Ltd."}
                   retrieved)
      "The record is updated in the database"))

(defn- assert-blocked-update
  [[response retrieved]]
  (is (http-not-found? response))
  (is (comparable? {:name "Microsoft, Inc"}
                   retrieved)
      "The record is not updated in the database"))

(deftest a-user-can-update-a-commodity-in-his-entity
  (assert-successful-update (update-a-commodity "john@doe.com")))

(deftest a-user-cannot-update-a-commodity-in-anothers-entity
  (assert-blocked-update (update-a-commodity "jane@doe.com")))

(defn- delete-a-commodity
  [email]
  (let [ctx (realize context)
        user (find-user ctx email)
        msft (find-commodity ctx "MSFT")
        response (-> (req/request :delete (path :api
                                                :commodities
                                                (:id msft)))
                     (add-auth user)
                     app)
        retrieved (coms/find msft)]
    [response retrieved]))

(defn- assert-successful-delete
  [[response retrieved]]
  (is (http-success? response))
  (is (nil? retrieved)
      "The commodity cannot be retrieved after delete"))

(defn- assert-blocked-delete
  [[response retrieved]]
  (is (http-not-found? response))
  (is retrieved
      "The commodity can be retrieved after failed delete"))

(deftest a-user-can-delete-a-commodity-in-his-entity
  (assert-successful-delete (delete-a-commodity "john@doe.com")))

(deftest a-user-cannot-delete-a-commodity-in-anothers-entity
  (assert-blocked-delete (delete-a-commodity "jane@doe.com")))
