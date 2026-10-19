(ns clj-money.authorization-test
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clj-money.entities.ref]
            [clj-money.db.ref]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-user
                                            find-account
                                            find-commodity]]
            [clj-money.authorization :as authorization]
            [clj-money.authorization.accounts]
            [clj-money.authorization.commodities]))

(use-fixtures :each reset-db)

(def ^:private grant-context
  (conj basic-context
        #:grant{:entity "Personal"
                :user "jane@doe.com"
                :permissions {:account #{:show :delete}}}))

(defn- allowed?
  [action]
  (authorization/allowed? (find-account "Checking")
                          action
                          (find-user "jane@doe.com")))

(deftest a-grant-allows-the-actions-it-permits
  (with-context grant-context
    (is (allowed? ::authorization/show))
    (testing "the grant's :delete permits ::authorization/destroy"
      (is (allowed? ::authorization/destroy)))))

(deftest a-grant-does-not-allow-the-actions-it-omits
  (with-context grant-context
    (is (not (allowed? ::authorization/create)))
    (is (not (allowed? ::authorization/update)))
    (testing "::authorization/recalculate requires :update"
      (is (not (allowed? ::authorization/recalculate))))))

(deftest a-grant-only-covers-its-resource-types
  (with-context grant-context
    (is (not (authorization/allowed? (find-commodity "USD")
                                     ::authorization/show
                                     (find-user "jane@doe.com"))))))
