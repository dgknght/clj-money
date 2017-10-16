(ns clj-money.permissions.attachments-test
  (:require [clojure.test :refer :all]
            [environ.core :refer [env]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clj-time.core :as t]
            [clj-factory.core :refer [factory]]
            [slingshot.test :refer :all]
            [clj-money.factories.user-factory]
            [clj-money.serialization :as serialization]
            [clj-money.authorization :refer [apply-scope
                                             allowed?
                                             tag-resource]]
            [clj-money.models.entities :as entities]
            [clj-money.models.grants :as grants]
            [clj-money.permissions.entities]
            [clj-money.test-helpers :refer [reset-db
                                            with-authentication
                                            find-commodity
                                            find-users
                                            find-entity]]))

(def storage-spec (env :db))

(use-fixtures :each (partial reset-db storage-spec))

; TODO Add attachment permission tests
