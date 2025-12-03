(ns clj-money.api.entities-test
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.pprint :refer [pprint]]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.web :refer [path]]
            [clj-money.util :as util]
            [clj-money.entities :as entities]
            [clj-money.db.ref]
            [clj-money.entities.ref]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [with-context
                                            find-user
                                            find-account
                                            find-entity]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.api.test-helper :refer [request
                                               parse-body
                                               ->json-entity-ref]]
            [clj-money.web.server :refer [app]]))

(use-fixtures :each reset-db)

(def ^:private create-context
  [(factory :user {:user/email "john@doe.com"})
   (factory :user {:user/email "jane@doe.com"})])

(defn- create-entity
  [& {:keys [user content-type body]
      :or {content-type "application/edn"
           body #:entity{:name "Personal"
                         :settings {:settings/inventory-method :fifo}}}}]
  (-> (request :post (path :api :entities)
               :user user
               :content-type content-type
               :body body)
      app
      parse-body))

(deftest a-user-can-create-an-entity
  (with-context create-context
    (let [user (find-user "john@doe.com")]
      (testing "default format (edn)"
        (let [{:as res
               :keys [parsed-body]}
              (create-entity :user user)]
          (is (http-success? res))
          (is (comparable? #:entity{:user (util/->entity-ref user)
                                    :name "Personal"
                                    :settings {:settings/inventory-method :fifo}}
                           (-> parsed-body :id entities/find))
              "The entity can be retrieved")))
      (testing "json format"
        (let [{:as res
               :keys [parsed-body]}
              (create-entity :user user
                             :content-type "application/json"
                             :body {:name "Alt-Personal"
                                    :settings {:inventory-method :fifo
                                               :_type :settings}
                                    :_type :entity})]
          (is (http-success? res))
          (is (comparable? {:user (->json-entity-ref user)
                            :name "Alt-Personal"
                            :settings {:inventoryMethod "fifo"}}
                           parsed-body)
              "The created entity is returned")
          (is (comparable? #:entity{:user (util/->entity-ref user)
                                    :name "Alt-Personal"
                                    :settings {:settings/inventory-method :fifo}}
                           (-> parsed-body :id entities/find))
              "The entity can be retrieved"))))))

(def ^:private list-context
  (conj create-context
        #:entity{:user "john@doe.com"
                 :name "Personal"}
        #:entity{:user "john@doe.com"
                 :name "Business"}
        #:commodity{:entity "Personal"
                    :type :currency
                    :name "US Dollar"
                    :symbol "USD"}
        #:account{:entity "Personal"
                  :type :expense
                  :name "Dining"}
        #:account{:entity "Personal"
                  :type :expense
                  :name "Groceries"}))

(defn- edit-an-entity
  [email & {:as opts
            :keys [content-type]
            :or {content-type "application/edn"}}]
  (let [changes (or (:changes opts)
                    {:entity/name "New Name"
                     :entity/settings
                     {:settings/monitored-accounts
                      #{(util/->entity-ref (find-account "Dining"))
                        (util/->entity-ref (find-account "Groceries"))}}})
        entity (find-entity "Personal")
        response (-> (request :patch (path :api :entities (:id entity))
                              :user (when email (find-user email))
                              :content-type content-type
                              :body changes)
                     app
                     parse-body)]
    [response (entities/find entity)]))

(defn- assert-successful-edit
  [[response retrieved]]
  (is (http-success? response))
  (is (comparable? {:entity/name "New Name"}
                   (:parsed-body response))
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
  (with-context list-context
    (testing "default format (edn)"
      (assert-successful-edit (edit-an-entity "john@doe.com")))
    (testing "json format"
      (let [monitors (map (comp util/->entity-ref
                                find-account)
                          ["Dining" "Groceries"])
            [res retrieved] (edit-an-entity
                              "john@doe.com"
                              :content-type "application/json"
                              :changes {:name "json name"
                                        :settings {:monitoredAccounts monitors
                                                   :_type :settings}
                                        :_type :entity})]
        (is (http-success? res))
        (is (comparable? {:name "json name"
                          :_type "entity"}
                         (:parsed-body res))
            "The response contains the updated entity")
        (is (= (set monitors)
               (->> (get-in res [:parsed-body :settings :monitoredAccounts])
                    (map (comp util/->entity-ref
                               entities/find
                               :id))
                    set)))
        (is (comparable? {:entity/name "json name"
                          :entity/settings {:settings/monitored-accounts (set monitors)}}
                         retrieved)
            "The updated entity can be retrieved")))))

(deftest a-user-cannot-edit-anothers-entity
  (with-context list-context
    (assert-blocked-edit (edit-an-entity "jane@doe.com"))))

(deftest an-unauthenticated-user-cannot-edit-an-entity
  (with-context list-context
    (let [entity (find-entity "Personal")
          [response] (edit-an-entity nil)]
      (is (http-unauthorized? response))
      (is (comparable? {:entity/name "Personal"}
                       (entities/find entity))
          "The retrieved value has not been changed."))))

(defn- get-a-list
  [email & {:keys [format]
            :or {format "application/edn"}}]
  (-> (request :get (path :api :entities)
               :content-type format
               :user (find-user email))
      app
      parse-body))

(defn- assert-successful-list
  [{:as response :keys [parsed-body]} & {:keys [expected]}]
  (is (http-success? response))
  (is (seq-of-maps-like? expected
                         parsed-body)
      "The body contains the entities"))

(defn- assert-blocked-list
  [{:as response :keys [parsed-body]}]
  (is (http-success? response))
  (is (empty? parsed-body) "The body is empty"))

(deftest a-user-can-get-a-list-of-his-entities
  (with-context list-context
    (testing "default format (edn)"
      (assert-successful-list (get-a-list "john@doe.com")
                              :expected [{:entity/name "Business"}
                                         {:entity/name "Personal"}]))
    (testing "json format"
      (assert-successful-list (get-a-list "john@doe.com" :format "application/json")
                              :expected [{:name "Business"}
                                         {:name "Personal"}]))))

(deftest a-user-cannot-get-a-list-of-anothers-entities
  (with-context list-context
    (assert-blocked-list (get-a-list "jane@doe.com"))))

(defn- delete-an-entity
  [email]
  (with-context list-context
    (let [entity (find-entity "Personal")]
      [(-> (request :delete (path :api :entities (:id entity))
                    :user (find-user email))
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
