(ns clj-money.images.s3
  (:require [clojure.tools.logging :as log]
            [cognitect.aws.client.api :as aws]
            [cognitect.aws.credentials :as credentials]
            [clj-money.images :as images])
  (:import [java.io ByteArrayInputStream]))

(defn- make-client
  [{:keys [host port access-key secret-key region]}]
  (aws/client
    (cond-> {:api :s3
             :region (or region "us-east-1")}
      (and host port)
      (assoc :endpoint-override {:protocol :http
                                 :hostname host
                                 :port port})
      (and access-key secret-key)
      (assoc :credentials-provider
             (credentials/basic-credentials-provider
               {:access-key-id access-key
                :secret-access-key secret-key})))))

(defn- anomaly?
  [result]
  (boolean (:cognitect.anomalies/category result)))

(defn- fetch*
  [uuid client bucket]
  (log/debugf "Fetching image %s from S3 bucket %s" uuid bucket)
  (let [result (aws/invoke client {:op :GetObject
                                   :request {:Bucket bucket
                                             :Key uuid}})]
    (when-not (anomaly? result)
      (.readAllBytes ^java.io.InputStream (:Body result)))))

(defn- stash*
  [uuid content client bucket]
  (log/debugf "Stashing image %s in S3 bucket %s" uuid bucket)
  (let [result (aws/invoke client {:op :PutObject
                                   :request {:Bucket bucket
                                             :Key uuid
                                             :Body (ByteArrayInputStream. ^bytes content)}})]
    (when (anomaly? result)
      (throw (ex-info "Failed to stash image in S3"
                      {:uuid uuid
                       :bucket bucket
                       :anomaly result})))
    uuid))

(defmethod images/reify-storage ::images/s3
  [{:keys [bucket] :as config}]
  (let [client (make-client config)]
    (reify images/Storage
      (fetch [_ uuid]
        (fetch* uuid client bucket))
      (stash [_ uuid content]
        (stash* uuid content client bucket)))))
