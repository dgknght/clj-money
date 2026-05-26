(ns clj-money.images.minio
  (:require [clojure.tools.logging :as log]
            [cognitect.aws.client.api :as aws]
            [cognitect.aws.credentials :as credentials]
            [clj-money.images :as images])
  (:import [java.io ByteArrayInputStream]))

(defn- make-client
  [{:keys [endpoint-host endpoint-port access-key secret-key region]}]
  (aws/client
    (cond-> {:api :s3
             :region (or region "us-east-1")}
      (and endpoint-host endpoint-port)
      (assoc :endpoint-override {:protocol :http
                                 :hostname endpoint-host
                                 :port endpoint-port})
      (and access-key secret-key)
      (assoc :credentials-provider
             (credentials/basic-credentials-provider
               {:access-key-id access-key
                :secret-access-key secret-key})))))

(defn- anomaly?
  [result]
  (boolean (:cognitect.anomalies/category result)))

(defmethod images/reify-storage ::images/minio
  [{:keys [bucket] :as config}]
  (let [client (make-client config)]
    (reify images/Storage
      (fetch [_ uuid]
        (log/debugf "Fetching image %s from MinIO bucket %s" uuid bucket)
        (let [result (aws/invoke client {:op :GetObject
                                         :request {:Bucket bucket
                                                   :Key uuid}})]
          (when-not (anomaly? result)
            (.readAllBytes ^java.io.InputStream (:Body result)))))
      (stash [_ uuid content]
        (log/debugf "Stashing image %s in MinIO bucket %s" uuid bucket)
        (let [result (aws/invoke client {:op :PutObject
                                          :request {:Bucket bucket
                                                    :Key uuid
                                                    :Body (ByteArrayInputStream. ^bytes content)}})]
          (when (anomaly? result)
            (throw (ex-info "Failed to stash image in MinIO"
                            {:uuid uuid
                             :bucket bucket
                             :anomaly result})))
          uuid)))))
