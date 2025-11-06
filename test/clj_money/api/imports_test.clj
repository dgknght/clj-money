(ns clj-money.api.imports-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [ring.mock.request :as req]
            [clj-factory.core :refer [factory]]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.test]
            [clj-money.json]
            [clj-money.io :refer [read-bytes]]
            [clj-money.util :as util]
            [clj-money.entities :as entities]
            [clj-money.factories.user-factory]
            [clj-money.test-helpers :refer [reset-db
                                            parse-edn-body]]
            [clj-money.api.test-helper :refer [add-auth
                                               parse-body
                                               request
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
          options {:lt-capital-gains-account "Investment Income/Long Term Gains"
                   :st-capital-gains-account "Investment Income/Short Term Gains"}

          {:as response
           {:keys [entity import]} :edn-body}
          (with-redefs [imports-api/launch-and-track (mock-launch-and-track calls)]
            (-> (req/request :post (path :api :imports))
                (merge
                  (build-multipart-request
                    {:entity-name "Personal"
                     :options (pr-str options)
                     :source-file-0 {:file source-file
                                     :content-type "application/gnucash"}}))
                (add-auth user)
                (req/header "Accept" "application/edn")
                app
                parse-edn-body))]
      (is (http-success? response))
      (is (comparable? #:entity{:name "Personal"
                                :user (util/->entity-ref user)}
                       entity)
          "The newly created entity is returned in the response")
      (is (comparable? #:import{:entity-name "Personal"
                                :options options
                                :user (util/->entity-ref user)}
                       import)
          "The newly created import is returned in the response")
      (is (seq-of-maps-like? [{:import/entity-name "Personal"}]
                             (entities/select {:import/user user}))
          "The new record can be retrieved from the database")
      (let [[c :as cs] @calls]
        (is (= 1 (count cs))
            "launch-and-track is called once")
        (is (comparable? {:import/entity-name "Personal"}
                         c)
            "The newly created import is passed to launch-and-track")))))

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
  [email & {:keys [content-type]
            :or {content-type "application/edn"}}]
  (with-context list-context
    (-> (request :get (path :api :imports)
                 :content-type content-type
                 :user (find-user email))
        app
        parse-body)))

(defn- assert-successful-list
  [{:as response :keys [edn-body parsed-body]}
   & {:keys [expected]
      :or {expected [#:import{:entity-name "Personal"}
                     #:import{:entity-name "Business"}]}}]
  (is (http-success? response))
  (let [body (or parsed-body edn-body)]
    (is (seq-of-maps-like? expected body)
        "The response contains the user's imports")))

(defn- assert-other-user-list
  [{:as response :keys [edn-body parsed-body]}]
  (is (http-success? response))
  (let [body (or parsed-body edn-body)]
    (is (empty? body)
        "No imports are included in the response")))

(deftest a-user-can-get-a-list-of-his-imports
  (assert-successful-list (get-a-list "john@doe.com"))
  (assert-successful-list
    (get-a-list "john@doe.com" :content-type "application/json")
    :expected [{:entityName "Personal"
                :_type "import"}
               {:entityName "Business"
                :_type "import"}]))

(deftest a-user-cannot-get-a-list-of-anothers-imports
  (assert-other-user-list (get-a-list "jane@doe.com")))

(defn- get-an-import
  [email & {:keys [content-type]
            :or {content-type "application/edn"}}]
  (with-context list-context
    (-> (request :get (path :api
                            :imports
                            (:id (find-import "Personal")))
                 :content-type content-type
                 :user (find-user email))
        app
        parse-body)))

(defn- assert-successful-get
  [{:as response :keys [edn-body parsed-body]}
   & {:keys [expected]
      :or {expected {:import/entity-name "Personal"}}}]
  (is (http-success? response))
  (let [body (or parsed-body edn-body)]
    (is (comparable? expected body)
        "The import is returned in the response")))

(defn- assert-blocked-get
  [response]
  (is (http-not-found? response)))

(deftest a-user-can-view-his-own-import
  (assert-successful-get (get-an-import "john@doe.com"))
  (assert-successful-get (get-an-import "john@doe.com" :content-type "application/json")
                         :expected {:entityName "Personal"
                                    :_type "import"}))

(deftest a-user-cannot-view-anothers-import
  (assert-blocked-get (get-an-import "jane@doe.com")))

(defn- delete-import
  [email & {:keys [content-type]
            :or {content-type "application/edn"}}]
  (with-context list-context
    (let [imp (find-import "Personal")]
      [(-> (request :delete (path :api :imports (:id imp))
                    :content-type content-type
                    :user (find-user email))
           app)
       (entities/find imp)])))

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
  (assert-successful-delete (delete-import "john@doe.com"))
  (assert-successful-delete (delete-import "john@doe.com" :content-type "application/json")))

(deftest a-user-cannot-delete-anothers-import
  (assert-blocked-delete (delete-import "jane@doe.com")))

(defn- start-import
  [email & {:keys [content-type]
            :or {content-type "application/edn"}}]
  (with-context list-context
    (let [imp (find-import "Personal")
          calls (atom [])
          response (with-redefs [imports-api/launch-and-track (mock-launch-and-track calls)]
                     (-> (request :patch (path :api :imports (:id imp))
                                  :content-type content-type
                                  :user (find-user email))
                         app
                         parse-body))]
      [response calls])))

(defn- assert-successful-start
  [[{:as response :keys [edn-body parsed-body]} calls]
   & {:keys [response-keys]
      :or {response-keys #{:entity :import}}}]
  (is (http-success? response))
  (let [body (or parsed-body edn-body)]
    (is (= response-keys (-> body keys set))
        "The response contains the relevant entity and import"))
  (let [[c :as cs] @calls]
    (is (= 1 (count cs))
        "Exactly one call is made to launch-and-track")
    (is (comparable? {:import/entity-name "Personal"}
                     c)
        "The specified import is started")))

(defn- assert-blocked-start
  [[response calls]]
  (is (http-not-found? response))
  (is (= 0 (count @calls))
      "No imports are started"))

(deftest a-user-can-start-his-import
  (assert-successful-start (start-import "john@doe.com"))
  (assert-successful-start (start-import "john@doe.com" :content-type "application/json")
                           :response-keys #{"entity" "import"}))

(deftest a-user-cannot-start-anothers-import
  (assert-blocked-start (start-import "jane@doe.com")))

(deftest halt-on-fatal-error
  (with-context list-context
    (let [_imp (find-import "Personal")])
    ))
