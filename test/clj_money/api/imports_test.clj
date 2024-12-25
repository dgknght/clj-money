(ns clj-money.api.imports-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.java.io :as io]
            [ring.mock.request :as req]
            [cheshire.core :as json]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test :refer [parse-json-body]]
            [dgknght.app-lib.test-assertions]
            [clj-money.util :as util]
            [clj-money.models :as models]
            [clj-money.factories.user-factory]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.api.test-helper :refer [add-auth
                                               build-multipart-request]]
            [clj-money.test-context :refer [with-context
                                            find-user
                                            find-import]]
            [clj-money.web.server :refer [app]]
            [clj-money.models.imports :as imports]
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
     :entity #:entity{:name (:entity-name imp)
                      :user (:import/user imp)}}))

(deftest a-user-can-create-an-import
  (with-context create-context
    (let [source-file (io/file (io/resource "fixtures/sample.gnucash"))
          user (find-user "john@doe.com")
          calls (atom [])

          {:as response
           {:keys [entity import]} :json-body}
          (with-redefs [imports-api/launch-and-track-import (mock-launch-and-track calls)]
            (-> (req/request :post (path :api :imports))
                (merge (build-multipart-request {:import/entity-name "Personal"
                                                 :import/source-file-0 {:file source-file
                                                                        :content-type "application/gnucash"}}))
                (add-auth user)
                app
                parse-json-body))]
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
        (is (comparable? {:import-entity/name "Personal"}
                         c)
          "The newly created import is passed to launch-and-track-import")))))

; (def ^:private list-context
;   (-> create-context
;       (update-in [:users] conj (factory :user {:email "jane@doe.com"}))
;       (assoc :imports [{:entity-name "Personal"}
;                        {:entity-name "Business"}])))
; 
; (defn- get-a-list
;   [email]
;   (let [ctx (realize list-context)
;         user (find-user ctx email)
;         response (-> (req/request :get (path :api :imports))
;                      (add-auth user)
;                      app)
;         body (json/parse-string (:body response) true)]
;     [response body]))
; 
; (defn- assert-successful-list
;   [[response body]]
;   (is (http-success? response))
;   (is (= #{"Personal" "Business"}
;          (->> body
;               (map :entity-name)
;               set))
;       "The response contains the user's imports"))
; 
; (defn- assert-other-user-list
;   [[response body]]
;   (is (http-success? response))
;   (is (not (some #(= "Personal" (:entity-name %)) body))
;       "The Personal import is not included in the result")
;   (is (not (some #(= "Business" (:entity-name %)) body))
;       "The Business import is not included in the result"))
; 
; (deftest a-user-can-get-a-list-of-his-imports
;   (assert-successful-list (get-a-list "john@doe.com")))
; 
; (deftest a-user-cannot-get-a-list-of-anothers-imports
;   (assert-other-user-list (get-a-list "jane@doe.com")))
; 
; (defn- get-an-import
;   [email]
;   (let [ctx (realize list-context)
;         user (find-user ctx email)
;         imp (find-import ctx "Personal")
;         response (-> (req/request :get (path :api :imports (:id imp)))
;                      (add-auth user)
;                      app)
;         body (json/parse-string (:body response) true)]
;     [response body]))
; 
; (defn- assert-successful-get
;   [[response body]]
;   (is (http-success? response))
;   (is (comparable? {:entity-name "Personal"} body)
;       "The import is returned in the response"))
; 
; (defn- assert-blocked-get
;   [[response]]
;   (is (http-not-found? response)))
; 
; (deftest a-user-can-view-his-own-import
;   (assert-successful-get (get-an-import "john@doe.com")))
; 
; (deftest a-user-cannot-view-anothers-import
;   (assert-blocked-get (get-an-import "jane@doe.com")))
; 
; (defn- delete-import
;   [email]
;   (let [ctx (realize list-context)
;         user (find-user ctx email)
;         imp (find-import ctx "Personal")
;         response (-> (req/request :delete (path :api :imports (:id imp)))
;                      (add-auth user)
;                      app)
;         retrieved (imports/find imp)]
;     [response retrieved]))
; 
; (defn- assert-successful-delete
;   [[response retrieved]]
;   (is (http-success? response))
;   (is (nil? retrieved)
;       "The import is not retrievable after delete"))
; 
; (defn- assert-blocked-delete
;   [[response retrieved]]
;   (is (http-not-found? response))
;   (is retrieved
;       "The import is retrievable after attempted delete"))
; 
; (deftest a-user-can-delete-his-import
;   (assert-successful-delete (delete-import "john@doe.com")))
; 
; (deftest a-user-cannot-delete-anothers-import
;   (assert-blocked-delete (delete-import "jane@doe.com")))
; 
; (defn- start-import
;   [email]
;   (let [ctx (realize list-context)
;         user (find-user ctx email)
;         imp (find-import ctx "Personal")
;         calls (atom [])
;         response (with-redefs [imports-api/launch-and-track-import (mock-launch-and-track calls)]
;                    (-> (req/request :patch (path :api :imports (:id imp)))
;                        (add-auth user)
;                        app))]
;     [response calls]))
; 
; (defn- assert-successful-start
;   [[response calls]]
;   (is (http-success? response))
;   (is (some #(= "Personal" (:entity-name %)) @calls)
;       "The import is started"))
; 
; (defn- assert-blocked-start
;   [[response calls]]
;   (is (http-not-found? response))
;   (is (not-any? #(= "Personal" (:entity-name %)) @calls)
;       "The import is not started"))
; 
; (deftest a-user-can-start-his-import
;   (assert-successful-start (start-import "john@doe.com")))
; 
; (deftest a-user-cannot-start-anothers-import
;   (assert-blocked-start (start-import "jane@doe.com")))
