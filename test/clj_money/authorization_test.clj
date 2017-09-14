(ns clj-money.authorization-test
  (:require [clojure.test :refer :all]
            [environ.core :refer [env]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [cemerick.friend :refer [current-authentication]]
            [clj-factory.core :refer [factory]]
            [clj-money.factories.user-factory]
            [clj-money.serialization :as serialization]
            [clj-money.validation :as validation]
            [clj-money.models.users :as users]
            [clj-money.models.entities :as entities]
            [clj-money.models.accounts :as accounts]
            [clj-money.authorization :refer [can?]]
            [clj-money.test-helpers :refer [reset-db
                                            find-account
                                            find-users
                                            find-entities]]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

(def authorization-context
  {:users [(assoc (factory :user) :email "john@doe.com")
           (assoc (factory :user) :email "jane@doe.com")]
   :entities [{:name "Personal"
               :user-id "john@doe.com"}
              {:name "Business"
               :user-id "jane@doe.com"}]})

(deftest a-user-can-manage-his-own-entities
  (let [context (serialization/realize storage-spec authorization-context)
        [john jane] (find-users context "john@doe.com" "jane@doe.com")
        [personal business] (find-entities context "Personal" "Business")]
    (with-redefs [current-authentication (fn [] john)]
      (doseq [action [:show :edit :update :delete]]
        (testing (format "a user has %s permission on his own entities" action)
          (is (can? action personal {})
              "A user has permission on his own entity")
          (is (not (can? action business {}))
              "A user does not have permission no someone else's entity"))))))
