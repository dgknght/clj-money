(ns clj-money.api.test-helper
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [ring.mock.request :as req]
            [ring.util.response :as res]
            [muuntaja.core :as muuntaja]
            [clj-money.web.auth :as auth])
  (:import [java.io File ByteArrayOutputStream]
           [com.fasterxml.jackson.core JsonGenerator]
           [org.apache.http.entity ContentType]
           [org.apache.http.entity.mime MultipartEntity]
           [org.apache.http.entity.mime.content StringBody FileBody]))

(defn add-auth
  [req user]
  (if user
    (req/header req "Authorization" (str "Bearer " (auth/make-token user)))
    req))

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

(defn no-content?
  [{:keys [status]}]
  (= 204 status))

(def has-content?
  (complement no-content?))

(defn parse-body
  [{:keys [body] :as res}]
  (if (has-content? res)
    (let [content-type (-> res
                           (res/get-header "content-type")
                           (string/split #";")
                           first)]
      (assoc res :parsed-body (try
                                (muuntaja/decode content-type body)
                                (catch Exception e
                                  {:error (ex-message e)}))))
    res))

(def muunstance
  (muuntaja/create
    (assoc-in muuntaja/default-options
              [:formats "application/json" :encoder-opts]
              {:encoders {clj_money.entities.CompositeID
                          (fn [x ^JsonGenerator gen]
                            (.writeString gen (str x)))}})))

(defn request
  [method path & {:keys [accept content-type body user]
                  :or {content-type "application/edn"}}]
  (let [accept (or accept content-type)]
    (cond-> (req/request method path)
      content-type (req/content-type content-type)
      accept (req/header :accept accept)
      body (assoc :body (muuntaja/encode muunstance content-type body))
      user (add-auth user))))
