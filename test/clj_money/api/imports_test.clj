(ns clj-money.api.imports-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.java.io :as io]
            [environ.core :refer [env]]
            [ring.mock.request :as req]
            [cheshire.core :as json]
            [clj-factory.core :refer [factory]]
            [clj-money.factories.user-factory]
            [clj-money.test-helpers :refer [reset-db
                                            selective=
                                            find-user
                                            find-import]]
            [clj-money.api.test-helper :refer [add-auth]]
            [clj-money.web.test-helpers :refer [assert-successful
                                                assert-not-found]]
            [clj-money.serialization :as serialization]
            [clj-money.x-platform.util :refer [path]]
            [clj-money.web.server :refer [app]]
            [clj-money.models.imports :as imports]
            [clj-money.api.imports :as imports-api])
  (:import [java.io File ByteArrayOutputStream]
           [org.apache.http.entity ContentType]
           [org.apache.http.entity.mime MultipartEntity]
           [org.apache.http.entity.mime.content StringBody FileBody]))

(use-fixtures :each (partial reset-db (env :db)))

(def ^:private create-context
  {:users [(factory :user {:email "john@doe.com"})]})

(defmulti ^:private add-part
  (fn [_ _ value]
    (type value)))

(defmethod ^:private add-part File
  [^MultipartEntity mpe k ^File file]
  (.addPart mpe k (FileBody. file
                             (ContentType/create "application/gnucash")
                             (.getName file))))

(defmethod ^:private add-part :default
  [^MultipartEntity mpe k v]
  (.addPart mpe k (StringBody. v)))

(defn- build-multipart-entity
  [params]
  (let [mpe (MultipartEntity.)]
    (doseq [[k v] params]
      (add-part mpe (name k) v))
    mpe))

(defn- build-multipart-request
  [params]
  (let [^MultipartEntity mpe (build-multipart-entity params)
        content-length (.getContentLength mpe)
        content-type (.getValue (.getContentType mpe))]
    {:body (let [out (ByteArrayOutputStream.)]
             (.writeTo mpe out)
             (.close out)
             (io/input-stream (.toByteArray out)))
     :content-length content-length
     :content-type content-type
     :headers {"content-type" content-type
               "content-length" (str content-length)}}))

(defn- mock-launch-and-track
  [calls]
  (fn
    [imp]
    (swap! calls conj imp)
    {:import imp
     :entity {:name (:entity-name imp)
              :user-id (:user-id imp)}}))

(deftest a-user-can-create-an-import
  (let [ctx (serialization/realize (env :db) create-context)
        user (find-user ctx "john@doe.com")
        source-file (io/file (io/resource "fixtures/sample.gnucash"))
        calls (atom [])
        response (with-redefs [imports-api/launch-and-track-import (mock-launch-and-track calls)]
                   (-> (req/request :post (path :api :imports))
                       (merge (build-multipart-request {:entity-name "Personal"
                                                        :source-file-0 source-file}))
                       (add-auth user)
                       app))
        body (json/parse-string (:body response) true)
        retrieved (imports/search (env :db) {:user-id (:id user)})]
    (assert-successful response)
    (is (selective= {:name "Personal"
                     :user-id (:id user)}
                    (:entity body))
        "The newly created entity is returned in the response")
    (is (selective= {:entity-name "Personal"
                     :user-id (:id user)}
                    (:import body))
        "The newly created import is returned in the response")
    (is (selective= {:entity-name "Personal"}
                    (first retrieved))
        "The new record can be retrieved from the database")
    (is (some #(= "Personal" (:entity-name %)) @calls)
        "The import is started")))

(def ^:private list-context
  (-> create-context
      (update-in [:users] conj (factory :user {:email "jane@doe.com"}))
      (assoc :imports [{:entity-name "Personal"}
                       {:entity-name "Business"}])))

(defn- get-a-list
  [email]
  (let [ctx (serialization/realize (env :db) list-context)
        user (find-user ctx email)
        response (-> (req/request :get (path :api :imports))
                     (add-auth user)
                     app)
        body (json/parse-string (:body response) true)]
    [response body]))

(defn- assert-successful-list
  [[response body]]
  (assert-successful response)
  (is (= #{"Personal" "Business"}
         (->> body
              (map :entity-name)
              set))
      "The response contains the user's imports"))

(defn- assert-other-user-list
  [[response body]]
  (assert-successful response)
  (is (not (some #(= "Personal" (:entity-name %)) body))
      "The Personal import is not included in the result")
  (is (not (some #(= "Business" (:entity-name %)) body))
      "The Business import is not included in the result"))

(deftest a-user-can-get-a-list-of-his-imports
  (assert-successful-list (get-a-list "john@doe.com")))

(deftest a-user-cannot-get-a-list-of-anothers-imports
  (assert-other-user-list (get-a-list "jane@doe.com")))

(defn- get-an-import
  [email]
  (let [ctx (serialization/realize (env :db) list-context)
        user (find-user ctx email)
        imp (find-import ctx "Personal")
        response (-> (req/request :get (path :api :imports (:id imp)))
                     (add-auth user)
                     app)
        body (json/parse-string (:body response) true)]
    [response body]))

(defn- assert-successful-get
  [[response body]]
  (assert-successful response)
  (is (selective= {:entity-name "Personal"} body)
      "The import is returned in the response"))

(defn- assert-blocked-get
  [[response]]
  (assert-not-found response))

(deftest a-user-can-view-his-own-import
  (assert-successful-get (get-an-import "john@doe.com")))

(deftest a-user-cannot-view-anothers-import
  (assert-blocked-get (get-an-import "jane@doe.com")))

(defn- delete-import
  [email]
  (let [ctx (serialization/realize (env :db) list-context)
        user (find-user ctx email)
        imp (find-import ctx "Personal")
        response (-> (req/request :delete (path :api :imports (:id imp)))
                     (add-auth user)
                     app)
        retrieved (imports/find-by-id (env :db) (:id imp))]
    [response retrieved]))

(defn- assert-successful-delete
  [[response retrieved]]
  (assert-successful response)
  (is (nil? retrieved)
      "The import is not retrievable after delete"))

(defn- assert-blocked-delete
  [[response retrieved]]
  (assert-not-found response)
  (is retrieved
      "The import is retrievable after attempted delete"))

(deftest a-user-can-delete-his-import
  (assert-successful-delete (delete-import "john@doe.com")))

(deftest a-user-cannot-delete-anothers-import
  (assert-blocked-delete (delete-import "jane@doe.com")))

(defn- start-import
  [email]
  (let [ctx (serialization/realize (env :db) list-context)
        user (find-user ctx email)
        imp (find-import ctx "Personal")
        calls (atom [])
        response (with-redefs [imports-api/launch-and-track-import (mock-launch-and-track calls)]
                   (-> (req/request :patch (path :api :imports (:id imp)))
                       (add-auth user)
                       app))]
    [response calls]))

(defn- assert-successful-start
  [[response calls]]
  (assert-successful response)
  (is (some #(= "Personal" (:entity-name %)) @calls)
      "The import is started"))

(defn- assert-blocked-start
  [[response calls]]
  (assert-not-found response)
  (is (not-any? #(= "Personal" (:entity-name %)) @calls)
      "The import is not started"))

(deftest a-user-can-start-his-import
  (assert-successful-start (start-import "john@doe.com")))

(deftest a-user-cannot-start-anothers-import
  (assert-blocked-start (start-import "jane@doe.com")))