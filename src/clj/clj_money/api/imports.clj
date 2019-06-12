(ns clj-money.api.imports
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [clojure.string :as string]
            [clojure.core.async :refer [go go-loop <! chan]]
            [cheshire.core :as json]
            [ring.util.response :refer [response
                                        status]]
            [environ.core :refer [env]]
            [cemerick.friend :as friend]
            [clj-money.io :refer [read-bytes]]
            [clj-money.validation :as validation]
            [clj-money.api :refer [delete-resource]]
            [clj-money.models.images :as images]
            [clj-money.import :refer [import-data]]
            [clj-money.import.gnucash]
            [clj-money.import.edn]
            [clj-money.models.imports :as imports]
            [clj-money.permissions.imports]))

(def ^:private expected-record-types
  [:budget :account :transaction :commodity :price])

(defn- launch-and-track-import
  [imp]
  (let [progress-chan (chan)]
    (go-loop [progress (<! progress-chan)]
             (when progress
               (imports/update (env :db)
                               (assoc imp :progress progress))
               (recur (<! progress-chan))))
    (import-data (env :db) imp progress-chan)))

(defn- infer-content-type
  [source-file]
  (let [filename (:filename source-file)
        stripped-filename (string/replace filename #"\.gz$" "")
        ext (second (re-matches #".*\.(.*)?$" stripped-filename))]
    (if (#{"gnucash" "edn"} ext)
      (format "application/%s" ext)
      (throw (ex-info (format
                        "Unable to infer the content type from the source file: %s"
                        (:filename source-file))
                      {:extension ext
                       :source-file source-file})))))

(defn- create-images
  [params user]
  (let [content-type (infer-content-type (:source-file-0 params))]
    (->> (range 10)
         (map #(format "source-file-%s" %))
         (map (comp params keyword))
         (take-while map?)
         (map #(images/find-or-create (env :db) {:user-id (:id user)
                                                 :content-type content-type
                                                 :original-filename (:filename %)
                                                 :body (read-bytes (:tempfile %))})))))
(defn create
  [{params :params}]
  (let [user (friend/current-authentication)
        images (create-images params user)]
    (if (not-any? #(validation/error-messages %) images)
      (let [imp (imports/create (env :db) {:user-id (:id user)
                                           :entity-name (:entity-name params)
                                           :image-ids (map :id images)})]
        (if (empty? (validation/error-messages imp))
          (let [{:keys [entity]} (launch-and-track-import imp)]
            (-> {:entity entity
                 :import imp}
                response
                (status 201)))
          (-> {:error (format "Unable to save the impport record. %s"
                              (->> imp
                                   validation/error-messages
                                   vals
                                   (mapcat identity)
                                   (string/join ", ")))}
              response
              (status 422))))
      (-> {:error (format "Unable to save the source file(s). %s"
                          (->> images
                               (map validation/error-messages)
                               (map vals)
                               flatten
                               (string/join ", ")))}
          response
          (status 422)))))

(defn show
  [{{id :id} :params}]
  ; TODO This needs authorization
  (let [imp (imports/find-by-id (env :db) id)]
    (response imp)))

(defn index
  [_]
  (response (imports/search (env :db) {:user-id (:id (friend/current-authentication))})))

(defn delete
  [{{id :id} :params}]
  (delete-resource id imports/find-by-id imports/delete))

(defn start
  [{{id :id} :params}]
  (let [imp (imports/find-by-id (env :db) id)]
    (launch-and-track-import imp)
    (-> imp response (status 200))))
