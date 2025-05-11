(ns clj-money.models.imports-test
  (:require [clojure.test :refer [deftest is use-fixtures testing]]
            [clojure.pprint :refer [pprint]]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.test-assertions]
            [clj-money.util :as util]
            [clj-money.models.ref]
            [clj-money.db.sql.ref]
            [clj-money.model-helpers :as helpers :refer [assert-invalid]]
            [clj-money.factories.user-factory]
            [clj-money.test-context :refer [with-context
                                            find-user
                                            find-image
                                            find-import]]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.models :as models]
            [clj-money.models.propagation :as prop]))

(use-fixtures :each reset-db)

(def import-context
  [(factory :user {:user/email "john@doe.com"})
   #:image{:original-filename "sample.gnucash"
           :content-type "application/gnucash"
           :user "john@doe.com"
           :body "resources/fixtures/sample.gnucash"}])

(defn attributes []
  #:import{:user (find-user "john@doe.com")
           :entity-name "Personal"
           :options {:lt-capital-gains-account "Investments/Long-Term Gains"
                     :st-capital-gains-account "Investments/Short-Term Gains"
                     :lt-capital-loss-account "Long-Term Losses"
                     :st-capital-loss-account "Short-Term Losses"}
           :images [(util/->model-ref (find-image "sample.gnucash"))]})

(defn- assert-created
  [attr]
  (helpers/assert-created attr :refs [:import/user]))

(deftest create-an-import
  (with-context import-context
    (assert-created (attributes))))

(def existing-imports-context
  (conj import-context
        #:import{:user "john@doe.com"
                 :entity-name "import entity"
                 :images ["sample.gnucash"]}))

(deftest get-a-list-of-imports
  (with-context existing-imports-context
    (is (seq-of-maps-like?
          [#:import{:entity-name "import entity"
                    :progress nil
                    :options nil
                    :entity-exists? false}]
          (models/select #:import{:user (find-user "john@doe.com")})))))

(deftest user-is-required
  (with-context import-context
    (assert-invalid (dissoc (attributes) :import/user)
                    {:import/user ["User is required"]})))

(deftest images-is-required
  (with-context import-context
    (assert-invalid (dissoc (attributes) :import/images)
                    {:import/images ["Images is required"]}))) ; TODO: Fix this grammar

(deftest entity-name-is-required
  (with-context import-context
    (assert-invalid (dissoc (attributes) :import/entity-name)
                    {:import/entity-name ["Entity name is required"]})))
 
(deftest update-an-import
  (with-context existing-imports-context
    (is (comparable? {:import/progress {:account {:total 20
                                                  :processed 0}}}
                     (-> (find-import "import entity")
                         (assoc :import/progress {:account {:total 20
                                                            :processed 0}})
                         models/put))
        "The return value contains the updated attributes")
    (is (comparable? {:import/progress {:account {:total 20
                                                  :processed 0}}}
                     (models/find (find-import "import entity")))
        "The retrieved value contains the updated attributes")))

(def ^:private delete-context
  (conj existing-imports-context
        #:image{:user "john@doe.com"
                :original-filename "sample_with_commodities.gnucash"
                :content-type "application/gnucash"
                :body "resources/fixtures/sample_with_commodities.gnucash"}
        #:import{:user "john@doe.com"
                 :entity-name "other entity"
                 :images ["sample_with_commodities.gnucash"]}
        #:import{:user "john@doe.com"
                 :entity-name "same entity"
                 :images ["sample.gnucash"]}))

(deftest delete-an-import
  (with-context delete-context
    (let [user (find-user "john@doe.com")]
      (testing "deleting an import deletes the associated files"
        (prop/delete-and-propagate (find-import "import entity"))
        (is (empty? (models/select #:import{:user user
                                            :entity-name "import entity"}))
            "The import record is removed")
        (is (empty? (models/select #:image{:user user
                                           :original-filename "sample.gnucash"}))
            "The image record is removed also"))
      (testing "deleting an import preserves associated files linked to other imports"
        (models/delete (find-import "same entity"))
        (is (empty? (models/select #:import{:user user
                                            :entity-name "same entity"}))
            "The import record is removed")
        (is (seq (models/select #:image{:user (:id user)
                                        :original-filename "sample_with_commodities.gnucash"}))
            "The image record is preserved")))))
