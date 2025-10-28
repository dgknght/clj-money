(ns clj-money.api.entities-test
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.pprint :refer [pprint]]
            [ring.mock.request :as req]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test :refer [parse-json-body]]
            [clj-money.entities :as entities]
            [clj-money.db.ref]
            [clj-money.entities.ref]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [with-context
                                            find-user
                                            find-entity]]
            [clj-money.test-helpers :refer [reset-db
                                            edn-body
                                            parse-edn-body]]
            [clj-money.api.test-helper :refer [add-auth]]
            [clj-money.web.server :refer [app]]))

(use-fixtures :each reset-db)

(def ^:private create-context
  [(factory :user {:user/email "john@doe.com"})
   (factory :user {:user/email "jane@doe.com"})])

(deftest a-user-can-create-an-entity
  (with-context create-context
    (let [user (find-user "john@doe.com")]
      (testing "default format (edn)"
        (let [response (-> (req/request :post (path :api :entities))
                           (edn-body #:entity{:name "Personal"
                                              :settings {:settings/inventory-method :fifo}})
                           (add-auth user)
                           app
                           parse-edn-body)]
          (is (http-success? response))
          (is (comparable? #:entity{:user (select-keys user [:id])
                                    :name "Personal"
                                    :settings {:settings/inventory-method :fifo}}
                           (entities/find (get-in response [:edn-body :id])
                                          :entity))
              "The entity can be retrieved")))
      (testing "json format"
        (let [response (-> (req/request :post (path :api :entities))
                           (req/json-body {:name "Personal"
                                           :settings {:inventory-method :fifo}})
                           (req/header :accept "application/json")
                           (add-auth user)
                           app
                           parse-json-body)]
          (is (http-success? response))
          (is (comparable? {:user (select-keys user [:id])
                            :name "Personal"
                            :settings {:inventory-method :fifo}}
                           (:json-body response))
              "The created entity is returned")
          (is (comparable? #:entity{:user (select-keys user [:id])
                                    :name "Personal"
                                    :settings {:settings/inventory-method :fifo}}
                           (entities/find (get-in response [:edn-body :id])
                                          :entity))
              "The entity can be retrieved"))))))

(def ^:private list-context
  (conj create-context
        #:entity{:user "john@doe.com"
                 :name "Personal"}
        #:entity{:user "john@doe.com"
                 :name "Business"}))

(defn- edit-an-entity
  [email]
  (with-context list-context
    (let [user (find-user email)
          entity (find-entity "Personal")
          response (-> (req/request :patch (path :api :entities (:id entity)))
                       (edn-body (-> entity
                                     (assoc :entity/name "New Name")
                                     (assoc-in [:entity/settings
                                                :settings/monitored-accounts]
                                               #{{:id 1}
                                                 {:id 2}})
                                     (select-keys [:entity/name
                                                   :entity/settings])))
                       (add-auth user)
                       app
                       parse-edn-body)]
      [response (entities/find entity)])))

(defn- assert-successful-edit
  [[response retrieved]]
  (is (http-success? response))
  (is (comparable? {:entity/name "New Name"}
                   (:edn-body response))
      "The updated entity is returned in the response")
  (is (comparable? {:entity/name "New Name"}
                   retrieved)
      "The retrieved value has the updated attributes"))

(defn- assert-blocked-edit
  [[response retrieved]]
  (is (http-not-found? response))
  (is (comparable? {:entity/name "Personal"}
                   retrieved)
      "The retrieved value has not been changed"))

(deftest a-user-can-edit-his-own-entity
  (assert-successful-edit (edit-an-entity "john@doe.com")))

(deftest a-user-cannot-edit-anothers-entity
  (assert-blocked-edit (edit-an-entity "jane@doe.com")))

(deftest an-unauthenticated-user-cannot-edit-an-entity
  (with-context list-context
    (let [entity (find-entity "Personal")
          response (-> (req/request :patch (path :api :entities (:id entity)))
                       (edn-body (-> entity
                                          (assoc :entity/name "New Name")
                                          (select-keys [:entity/name
                                                        :entity/settings])))
                       app)]
      (is (http-unauthorized? response))
      (is (comparable? {:entity/name "Personal"}
                       (entities/find entity))
          "The retrieved value has not been changed."))))

(defn- get-a-list
  [email & {:keys [format parse]
            :or {format "application/edn"
                 parse parse-edn-body}}]
  (-> (req/request :get (path :api :entities))
      (req/content-type format)
      (req/header "Accept" format)
      (add-auth (find-user email))
      app
      parse))

(defn- assert-successful-list
  [response & {:keys [expected response-key]}]
  (is (http-success? response))
  (is (seq-of-maps-like? expected
                         (response response-key))
      "The body contains the correct entities"))

(defn- assert-blocked-list
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (empty? edn-body) "The body is empty"))

(deftest a-user-can-get-a-list-of-his-entities
  (with-context list-context
    (testing "default format (edn)"
      (assert-successful-list (get-a-list "john@doe.com")
                              :response-key :edn-body
                              :expected [{:entity/name "Business"}
                                         {:entity/name "Personal"}]))
    (testing "json format"
      (assert-successful-list (get-a-list "john@doe.com"
                                          :format "application/json"
                                          :parse parse-json-body)
                              :response-key :json-body
                              :expected [{:name "Business"}
                                         {:name "Personal"}]))))

(deftest a-user-cannot-get-a-list-of-anothers-entities
  (with-context list-context
    (assert-blocked-list (get-a-list "jane@doe.com"))))

(defn- delete-an-entity
  [email]
  (with-context list-context
    (let [entity (find-entity "Personal")]
      [(-> (req/request :delete
                        (path :api
                              :entities
                              (:id entity)))
           (add-auth (find-user email))
           app)
       (entities/find entity)])))

(defn- assert-successful-delete
  [[response retrieved]]
  (is (http-success? response))
  (is (nil? retrieved) "The entity is not available after delete"))

(defn- assert-blocked-delete
  [[response retrieved]]
  (is (http-not-found? response))
  (is retrieved "The entity is still available after failed delete"))

(deftest a-user-can-delete-his-own-entity
  (assert-successful-delete (delete-an-entity "john@doe.com")))

(deftest a-user-cannot-delete-anothers-entity
  (assert-blocked-delete (delete-an-entity "jane@doe.com")))
