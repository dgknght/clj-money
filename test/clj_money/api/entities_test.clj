(ns clj-money.api.entities-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [environ.core :refer [env]]
            [ring.mock.request :as req]
            [cheshire.core :as json]
            [clj-factory.core :refer [factory]]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [realize
                                            find-user
                                            find-entity]]
            [clj-money.test-helpers :as h :refer [selective=]]
            [clj-money.web.test-helpers :refer [assert-successful
                                                assert-not-found
                                                assert-unauthorized]]
            [clj-money.api.test-helper :refer [add-auth]]
            [clj-money.web.server :refer [app]]
            [clj-money.models.entities :as entities]
            [clj-money.x-platform.util :refer [path]]))

(use-fixtures :each (partial h/reset-db (env :db)))

(def ^:private create-context
  {:users [(factory :user {:email "john@doe.com"})
           (factory :user {:email "jane@doe.com"})]})

(deftest a-user-can-create-an-entity
  (let [ctx (realize (env :db) create-context)
        user (find-user ctx "john@doe.com")
        response (-> (req/request :post (path :api :entities))
                     (req/json-body {:name "Personal"
                                     :settings {:inventory-method :fifo}})
                     (add-auth user)
                     app)
        retrieved (entities/select (env :db) {:user-id (:id user)})]
    (assert-successful response)
    (is (selective= {:user-id (:id user)
                     :name "Personal"
                     :settings {:inventory-method :fifo}}
                    (first retrieved)))))

(def ^:private list-context
  (assoc create-context :entities [{:user-id "john@doe.com"
                                    :name "Personal"}
                                   {:user-id "john@doe.com"
                                    :name "Business"}]))

(defn- edit-an-entity
  [email]
  (let [ctx (realize (env :db) list-context)
        user (find-user ctx email)
        entity (find-entity ctx "Personal")
        response (-> (req/request :patch (path :api :entities (:id entity)))
                     (req/json-body (-> entity
                                        (assoc :name "New Name")
                                        (assoc-in [:settings :monitored-account-ids] #{1 2})
                                        (select-keys [:name :settings])))
                     (add-auth user)
                     app)
        body (json/parse-string (:body response) true)
        retrieved (entities/find-by-id (env :db) (:id entity))]
    [response body retrieved]))

(defn- assert-successful-edit
  [[response body retrieved]]
  (assert-successful response)
  (is (selective= {:name "New Name"}
                  body)
      "The updated entity is returned in the response")
  (is (selective= {:name "New Name"}
                  retrieved)
      "The retrieved value has the updated attributes"))

(defn- assert-blocked-edit
  [[response _ retrieved]]
  (assert-not-found response)
  (is (selective= {:name "Personal"}
                  retrieved)
      "The retrieved value has not been changed"))

(deftest a-user-can-edit-his-own-entity
  (assert-successful-edit (edit-an-entity "john@doe.com")))

(deftest a-user-cannot-edit-anothers-entity
  (assert-blocked-edit (edit-an-entity "jane@doe.com")))

(deftest an-unauthenticated-user-cannot-edit-an-entity
  (let [ctx (realize (env :db) list-context)
        entity (find-entity ctx "Personal")
        response (-> (req/request :patch (path :api :entities (:id entity)))
                     (req/json-body (-> entity
                                        (assoc :name "New Name")
                                        (select-keys [:name :settings])))
                     app)
        retrieved (entities/find-by-id (env :db) (:id entity))]
    (assert-unauthorized response)
    (is (selective= {:name "Personal"}
                    retrieved)
        "The retrieved value has not been changed.")))

(defn- get-a-list
  [email]
  (let  [ctx (realize (env :db) list-context)
         user (find-user ctx email)
         response (-> (req/request :get (path :api :entities))
                      (add-auth user)
                      app)
         body (json/parse-string (:body response) true)]
    [response body]))

(defn- assert-successful-list
  [[response body]]
  (assert-successful response)
  (is (= #{"Personal" "Business"} (set (map :name body)))
      "The body contains the correct entities"))

(defn- assert-blocked-list
  [[response body]]
  (assert-successful response)
  (is (empty? body) "The body is empty"))

(deftest a-user-can-get-a-list-of-his-entities
  (assert-successful-list (get-a-list "john@doe.com")))

(deftest a-user-cannot-get-a-list-of-anothers-entities
  (assert-blocked-list (get-a-list "jane@doe.com")))

(defn- delete-an-entity
  [email]
  (let  [ctx (realize (env :db) list-context)
         user (find-user ctx email)
         entity (find-entity ctx "Personal")
         response (-> (req/request :delete (path :api :entities (:id entity)))
                      (add-auth user)
                      app)
         retrieved (entities/find-by-id (env :db) (:id entity))]
    [response retrieved]))

(defn- assert-successful-delete
  [[response retrieved]]
  (assert-successful response)
  (is (nil? retrieved) "The entity is not available after delete"))

(defn- assert-blocked-delete
  [[response retrieved]]
  (assert-not-found response)
  (is retrieved "The entity is still available after failed delete"))

(deftest a-user-can-delete-his-own-entity
  (assert-successful-delete (delete-an-entity "john@doe.com")))

(deftest a-user-cannot-delete-anothers-entity
  (assert-blocked-delete (delete-an-entity "jane@doe.com")))
