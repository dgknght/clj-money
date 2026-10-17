(ns clj-money.api.images-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [clojure.java.io :as io]
            [ring.mock.request :as req]
            [dgknght.app-lib.web :refer [path]]
            [dgknght.app-lib.test-assertions]
            [dgknght.app-lib.test]
            [clj-money.entities.ref]
            [clj-money.db.ref]
            [clj-money.test-helpers :refer [reset-db]]
            [clj-money.api.test-helper :refer [add-auth
                                               parse-body
                                               build-multipart-request]]
            [clj-money.test-context :refer [with-context
                                            basic-context
                                            find-user]]
            [clj-money.entities :as entities]
            [clj-money.web.test-handler :refer [app]]))

(use-fixtures :each reset-db)

(defn- upload
  [email params]
  (-> (req/request :post (path :api :images))
      (merge (build-multipart-request params))
      (add-auth (find-user email))
      (req/header "Accept" "application/edn")
      app
      parse-body))

(deftest a-user-can-upload-a-statement
  (with-context basic-context
    (let [{:as response :keys [parsed-body]}
          (upload "john@doe.com"
                  {:file {:file (io/file (io/resource "fixtures/statements/checking.pdf"))
                          :content-type "application/pdf"}})]
      (is (http-created? response))
      (is (comparable? #:image{:original-filename "checking.pdf"
                               :content-type "application/pdf"}
                       parsed-body)
          "The response is the new image")
      (is (comparable? #:image{:user (select-keys (find-user "john@doe.com") [:id])}
                       (entities/find (:id parsed-body)))
          "The image belongs to the user"))))

(deftest a-file-is-required
  (with-context basic-context
    (is (http-bad-request? (upload "john@doe.com" {:caption "nothing"})))))
