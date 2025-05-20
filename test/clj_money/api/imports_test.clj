(ns clj-money.api.imports-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [ring.mock.request :as req]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test-assertions]
            [clj-money.io :refer [read-bytes]]
            [clj-money.util :as util]
            [clj-money.models :as models]
            [clj-money.factories.user-factory]
            [clj-money.test-helpers :refer [reset-db
                                            parse-edn-body]]
            [clj-money.api.test-helper :refer [add-auth
                                               build-multipart-request]]
            [clj-money.test-context :refer [with-context
                                            find-user
                                            find-import]]
            [clj-money.web.server :refer [app]]
            [clj-money.api.imports :as imports-api]))

(use-fixtures :each reset-db)

(def ^:private create-context
  [(factory :user {:user/email "john@doe.com"})])

(defn- mock-launch-and-track
  [calls]
  (fn
    [imp]
    (swap! calls conj imp)
    {:import imp
     :entity #:entity{:name (:import/entity-name imp)
                      :user (:import/user imp)}}))

(deftest a-user-can-create-an-import
  (with-context create-context
    (let [source-file (io/file (io/resource "fixtures/sample.gnucash"))
          user (find-user "john@doe.com")
          calls (atom [])

          {:as response
           {:keys [entity import]} :edn-body}
          (with-redefs [imports-api/launch-and-track-import (mock-launch-and-track calls)]
            (-> (req/request :post (path :api :imports))
                (merge (build-multipart-request {:import/entity-name "Personal"
                                                 :import/source-file-0 {:file source-file
                                                                        :content-type "application/gnucash"}}))
                (add-auth user)
                (req/header "Accept" "application/edn")
                app
                parse-edn-body))]
      (is (http-success? response))
      (is (comparable? #:entity{:name "Personal"
                                :user (util/->model-ref user)}
                       entity)
          "The newly created entity is returned in the response")
      (is (comparable? #:import{:entity-name "Personal"
                                :user (util/->model-ref user)}
                       import)
          "The newly created import is returned in the response")
      (is (seq-of-maps-like? [{:import/entity-name "Personal"}]
                             (models/select {:import/user user}))
          "The new record can be retrieved from the database")
      (let [[c :as cs] @calls]
        (is (= 1 (count cs))
            "launch-and-track-import is called once")
        (is (comparable? {:import/entity-name "Personal"}
                         c)
          "The newly created import is passed to launch-and-track-import")))))

(def ^:private list-context
  (conj create-context
        (factory :user {:user/email "jane@doe.com"})
        #:image{:user "john@doe.com"
                :original-filename "sample.gnucash"
                :content-type "application/gnucash"
                :content (read-bytes (io/input-stream "resources/fixtures/sample.gnucash"))}
        #:import{:entity-name "Personal"
                 :user "john@doe.com"
                 :images ["sample.gnucash"]}
        #:import{:entity-name "Business"
                 :user "john@doe.com"
                 :images ["sample.gnucash"]}))

(defn- get-a-list
  [email]
  (with-context list-context
    (-> (req/request :get (path :api :imports))
        (add-auth (find-user email))
        app
        parse-edn-body)))

(defn- assert-successful-list
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (seq-of-maps-like? [#:import{:entity-name "Personal"}
                          #:import{:entity-name "Business"}]
                         edn-body)
      "The response contains the user's imports"))

(defn- assert-other-user-list
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (empty? edn-body)
      "No imports are included in the response"))

(deftest a-user-can-get-a-list-of-his-imports
  (assert-successful-list (get-a-list "john@doe.com")))

(deftest a-user-cannot-get-a-list-of-anothers-imports
  (assert-other-user-list (get-a-list "jane@doe.com")))

(defn- get-an-import
  [email]
  (with-context list-context
    (-> (req/request :get (path :api
                                :imports
                                (:id (find-import "Personal"))))
        (add-auth (find-user email))
        app
        parse-edn-body)))

(defn- assert-successful-get
  [{:as response :keys [edn-body]}]
  (is (http-success? response))
  (is (comparable? {:import/entity-name "Personal"} edn-body)
      "The import is returned in the response"))

(defn- assert-blocked-get
  [response]
  (is (http-not-found? response)))

(deftest a-user-can-view-his-own-import
  (assert-successful-get (get-an-import "john@doe.com")))

(deftest a-user-cannot-view-anothers-import
  (assert-blocked-get (get-an-import "jane@doe.com")))

(defn- delete-import
  [email]
  (with-context list-context
    (let [imp (find-import "Personal")]
      [(-> (req/request :delete (path :api :imports (:id imp)))
           (add-auth (find-user email))
           app)
       (models/find imp)])))

(defn- assert-successful-delete
  [[response retrieved]]
  (is (http-success? response))
  (is (nil? retrieved)
      "The import is not retrievable after delete"))

(defn- assert-blocked-delete
  [[response retrieved]]
  (is (http-not-found? response))
  (is retrieved
      "The import is retrievable after attempted delete"))

(deftest a-user-can-delete-his-import
  (assert-successful-delete (delete-import "john@doe.com")))

(deftest a-user-cannot-delete-anothers-import
  (assert-blocked-delete (delete-import "jane@doe.com")))

(defn- start-import
  [email]
  (with-context list-context
    (let [imp (find-import "Personal")
          calls (atom [])
          response (with-redefs [imports-api/launch-and-track-import (mock-launch-and-track calls)]
                     (-> (req/request :patch (path :api :imports (:id imp)))
                         (add-auth (find-user email))
                         app
                         parse-edn-body))]
      [response calls])))

(defn- assert-successful-start
  [[{:as response :keys [edn-body]} calls]]
  (is (http-success? response))
  (is (= #{:entity :import} (-> edn-body keys set))
      "The response contains the relevant entity and import")
  (let [[c :as cs] @calls]
    (is (= 1 (count cs))
        "Exactly one call is made to launch-and-track-import")
    (is (comparable? {:import/entity-name "Personal"}
                     c)
        "The specified import is started")))

(defn- assert-blocked-start
  [[response calls]]
  (is (http-not-found? response))
  (is (= 0 (count @calls))
      "No imports are started"))

(deftest a-user-can-start-his-import
  (assert-successful-start (start-import "john@doe.com")))

(deftest a-user-cannot-start-anothers-import
  (assert-blocked-start (start-import "jane@doe.com")))

(deftest halt-on-fatal-error
  (with-context list-context
    (let [_imp (find-import "Personal")])
    ))
