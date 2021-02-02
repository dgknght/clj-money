(ns clj-money.api.test-helper
  (:require [clojure.java.io :as io]
            [ring.mock.request :as req]
            [cheshire.core :as json]
            [clj-money.web.auth :as auth])
  (:import [java.io File ByteArrayOutputStream]
           [org.apache.http.entity ContentType]
           [org.apache.http.entity.mime MultipartEntity]
           [org.apache.http.entity.mime.content StringBody FileBody]))

(defn add-auth
  [req user]
  {:pre [user]}
  (req/header req "Authorization" (str "Bearer " (auth/make-token user))))

(defmulti ^:private add-part
  (fn [_ _ value]
    (when (= File (type (:file value)))
      :file)))

(defmethod ^:private add-part :file
  [^MultipartEntity mpe k {:keys [content-type file]
                           :or {content-type "text/plain"}}]
  (.addPart mpe k (FileBody. ^File file
                             (ContentType/create content-type)
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

(defn build-multipart-request
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

(defn parse-json-body
  [{:keys [body] :as res}]
  (assoc res :json-body (json/parse-string body true)))
