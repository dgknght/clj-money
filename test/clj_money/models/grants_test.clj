(ns clj-money.models.grants-test
  (:require [clojure.test :refer [is]]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.test]
            [clj-money.factories.user-factory]
            [clj-money.models.ref]
            [clj-money.db.ref]
            [clj-money.models :as models]
            [clj-money.model-helpers :as helpers :refer [assert-deleted]]
            [clj-money.test-context :refer [with-context
                                            find-entity
                                            find-user
                                            find-grant]]
            [clj-money.test-helpers :refer [dbtest]]))

(def ^:private grant-context
  (conj (mapv #(factory :user {:user/email %})
              ["john@doe.com" "jane@doe.com"])
        #:entity{:name "Business"
                 :user "john@doe.com"}
        #:commodity{:name "US Dollar"
                    :entity "Business"
                    :type :currency
                    :symbol "USD"}))

(defn- assert-created
  [attr]
  (helpers/assert-created attr :refs [:grant/entity :grant/user]))

(dbtest create-a-grant
  (with-context grant-context
    (assert-created #:grant{:entity (find-entity "Business") 
                            :user (find-user "jane@doe.com")
                            :permissions {:account #{:index :show}}})))

(def ^:private existing-grant-context
  (conj grant-context
        #:grant{:user "jane@doe.com"
                :entity "Business"
                :permissions {:account #{:index :show}
                              :commodity #{:index :update}}}))

(dbtest update-a-grant
        (with-context existing-grant-context
          (let [result (-> (find-grant ["Business" "jane@doe.com"])
                           (update-in [:grant/permissions]
                                      assoc :transaction #{:index :show})
                           (update-in [:grant/permissions :account] disj :index)
                           (update-in [:grant/permissions] dissoc :commodity)
                           models/put)]
            (is (= {:transaction #{:index :show}
                    :account #{:show}}
                   (:grant/permissions result))
                "The returned value has the specified attributes") 
            (is (= {:transaction #{:index :show}
                    :account #{:show}}
                   (:grant/permissions (models/find result)))
                "The retrieved value has the specified attributes"))))

(dbtest delete-a-grant
  (with-context existing-grant-context
    (assert-deleted (find-grant ["Business" "jane@doe.com"]))))
