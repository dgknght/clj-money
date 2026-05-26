(ns clj-money.images.s3-test
  (:require [clojure.test :refer [deftest is]]
            [cognitect.aws.client.api :as aws]
            [clj-money.images :as images]
            [clj-money.images.s3]
            [clj-money.images.storage-contract :as contract])
  (:import [java.io ByteArrayInputStream]))

(def ^:private test-config
  {:clj-money.images/strategy :clj-money.images/s3
   :bucket "test-bucket"
   :endpoint-host "localhost"
   :endpoint-port 9000
   :access-key "minioadmin"
   :secret-key "minioadmin"})

; ---------------------------------------------------------------
; Behavioral contract (stateful in-memory S3 mock)
; ---------------------------------------------------------------

(defn- make-s3-mock
  "Returns a fn compatible with aws/invoke that simulates an S3 bucket in memory."
  []
  (let [store (atom {})]
    (fn [_client {:keys [op request]}]
      (case op
        :PutObject
        (do (swap! store assoc (:Key request) (-> request :Body .readAllBytes))
            {})
        :GetObject
        (if-let [content (get @store (:Key request))]
          {:Body (ByteArrayInputStream. content)}
          {:cognitect.anomalies/category :cognitect.anomalies/not-found})))))

(deftest s3-storage-behavioral-contract
  (with-redefs [aws/invoke (make-s3-mock)]
    (contract/run-contract (images/reify-storage test-config))))

; ---------------------------------------------------------------
; Implementation-specific unit tests (aws/invoke is mocked)
; ---------------------------------------------------------------

(deftest fetch-invokes-get-object
  (let [content (.getBytes "image data")
        calls (atom [])]
    (with-redefs [aws/invoke (fn [_client req]
                               (swap! calls conj req)
                               {:Body (ByteArrayInputStream. content)})]
      (let [storage (images/reify-storage test-config)]
        (images/fetch storage "my-uuid")
        (is (= [{:op :GetObject
                 :request {:Bucket "test-bucket"
                           :Key "my-uuid"}}]
               @calls)
            "invokes :GetObject with the configured bucket and uuid as key")))))

(deftest fetch-returns-nil-on-not-found-anomaly
  (with-redefs [aws/invoke (constantly {:cognitect.anomalies/category
                                        :cognitect.anomalies/not-found})]
    (let [storage (images/reify-storage test-config)]
      (is (nil? (images/fetch storage "missing-uuid"))
          "returns nil when S3 reports not-found"))))

(deftest stash-invokes-put-object
  (let [content (.getBytes "image data")
        calls (atom [])]
    (with-redefs [aws/invoke (fn [_client req]
                               (swap! calls conj req)
                               {})]
      (let [storage (images/reify-storage test-config)]
        (images/stash storage "my-uuid" content)
        (is (= 1 (count @calls))
            "invoke called once")
        (let [{:keys [op request]} (first @calls)]
          (is (= :PutObject op)
              "uses the :PutObject operation")
          (is (= "test-bucket" (:Bucket request))
              "uses the configured bucket")
          (is (= "my-uuid" (:Key request))
              "uses the uuid as the object key")
          (is (= (seq content) (seq (.readAllBytes (:Body request))))
              "streams the content as the request body"))))))

(deftest stash-returns-uuid
  (with-redefs [aws/invoke (constantly {})]
    (let [storage (images/reify-storage test-config)]
      (is (= "my-uuid" (images/stash storage "my-uuid" (.getBytes "data")))
          "returns the uuid after a successful put"))))

(deftest stash-throws-on-s3-anomaly
  (with-redefs [aws/invoke (constantly {:cognitect.anomalies/category :cognitect.anomalies/fault
                                        :cognitect.anomalies/message "internal error"})]
    (let [storage (images/reify-storage test-config)]
      (is (thrown? clojure.lang.ExceptionInfo
                   (images/stash storage "my-uuid" (.getBytes "data")))
          "throws ExceptionInfo when S3 returns an anomaly"))))
