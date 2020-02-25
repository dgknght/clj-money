(ns clj-money.api.imports
  (:refer-clojure :exclude [update])
  (:require [clojure.string :as string]
            [clojure.core.async :refer [go-loop <! chan]]
            [ring.util.response :refer [response
                                        status]]
            [compojure.core :refer [defroutes GET POST PATCH DELETE]]
            [environ.core :refer [env]]
            [clj-money.io :refer [read-bytes]]
            [clj-money.validation :as validation]
            [clj-money.api :refer [delete-resource]]
            [clj-money.models.images :as images]
            [clj-money.import :refer [import-data]]
            [clj-money.import.gnucash]
            [clj-money.import.edn]
            [clj-money.models.imports :as imports]
            [clj-money.authorization :refer [authorize]]
            [clj-money.permissions.imports]))

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
  {:pre [source-file]}

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
(defn- create
  [{:keys [params authenticated]}]
  (let [images (create-images params authenticated)]
    (if (not-any? #(validation/error-messages %) images)
      (let [imp (imports/create (env :db) {:user-id (:id authenticated)
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
                                   flatten
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

(defn- show
  [{:keys [params authenticated]}]
  (let [imp (authorize (imports/find-by-id (env :db) (:id params))
                       :show
                       authenticated)]
    (response imp)))

(defn- index
  [{:keys [authenticated]}]
  (response (imports/search (env :db) {:user-id (:id authenticated)})))

(defn- delete
  [{:keys [params authenticated]}]
  (delete-resource (:id params) authenticated imports/find-by-id imports/delete))

(defn- start
  [{:keys [params authenticated]}]
  (let [imp (authorize (imports/find-by-id (env :db) (:id params))
                       :update
                       authenticated)]
    (launch-and-track-import imp)
    (-> imp response (status 200))))

(defroutes routes
  (GET "/api/imports" req (index req))
  (POST "/api/imports" req (create req))
  (GET "/api/imports/:id" req (show req))
  (PATCH "/api/imports/:id" req (start req))
  (DELETE "/api/imports/:id" req (delete req)))
