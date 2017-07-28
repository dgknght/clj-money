(ns clj-money.api.imports
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [clojure.string :as string]
            [clojure.core.async :refer [go go-loop <! chan]]
            [ring.util.response :refer [response]]
            [environ.core :refer [env]]
            [cemerick.friend :as friend]
            [clj-money.io :refer [read-bytes]]
            [clj-money.validation :as validation]
            [clj-money.models.images :as images]
            [clj-money.import :refer [import-data]]
            [clj-money.import.gnucash]
            [clj-money.models.imports :as imports]))

(def ^:private expected-record-types
  [:budget :account :transaction :commodity :price])

(defn- progress-complete?
  [progress]
  (let [filtered (select-keys progress expected-record-types)]
    (and (seq filtered)
         (every? (fn [[_ {:keys [total imported]}]]
                   (= total imported))
                 filtered))))

; TODO Maybe it would be better if the channel received
; a special message that indicated the file had been
; completely read?
(defn- launch-and-track-import
  [import]
  (let [progress-chan (chan)]
    (go-loop [continue true]
             (when continue
               (let [progress (<! progress-chan)]
                 (imports/update (env :db)
                                 (assoc import :progress progress))
                 (recur (not (progress-complete? progress))))))
    (go (import-data (env :db) import progress-chan))))

(defn- infer-content-type
  [source-file]
  (let [ext (second (re-matches #".*\.(.*)$" (:filename source-file)))]
    (if (= "gnucash" ext)
      "application/gnucash"
      (throw (ex-info "Unable to infer the content type from the source file" {:extension ext
                                                                               :source-file source-file})))))

(defn create
  [{params :params}]
  (let [user (friend/current-authentication)
        content-type (infer-content-type (:source-file params))
        image (images/find-or-create (env :db) {:user-id (:id user)
                                                :content-type content-type
                                                :original-filename (-> params :source-file :filename)
                                                :body (read-bytes (-> params :source-file :tempfile))})]
    (if (empty? (validation/error-messages image))
      (let [import (imports/create (env :db) {:user-id (:id user)
                                              :entity-name (:entity-name params)
                                              :image-id (:id image)})]
        (if (empty? (validation/error-messages import))
          (do
            (launch-and-track-import import)
            (response {:import import}))
          (response {:error (format "Unable to save the import record. %s"
                                    (->> import
                                         validation/error-messages
                                         vals
                                         (mapcat identity)
                                         (string/join ", ")))})))
      (response {:error (format "Unable to save the source file. %s"
                                (->> image
                                     validation/error-messages
                                     vals
                                     (mapcat identity)
                                     (string/join ", ")))}))))

(defn show
  [{{id :id} :params}]
  (->> (Integer. id)
       (imports/find-by-id (env :db))
       response))
