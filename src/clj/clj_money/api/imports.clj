(ns clj-money.api.imports
  (:refer-clojure :exclude [update])
  (:require [clojure.string :as string]
            [clojure.core.async :refer [go-loop <! chan]]
            [cheshire.core :as json]
            [compojure.core :refer [defroutes GET POST PATCH DELETE]]
            [clj-money.util :refer [update-in-if]]
            [clj-money.io :refer [read-bytes]]
            [clj-money.validation :as validation]
            [clj-money.api :refer [->response
                                   not-found]]
            [clj-money.models :as models]
            [clj-money.models.images :as images]
            [clj-money.import :refer [import-data]]
            [clj-money.import.gnucash]
            [clj-money.import.edn]
            [clj-money.models.imports :as imports]
            [clj-money.authorization :refer [authorize +scope] :as authorization]
            [clj-money.authorization.imports]))

(defn- launch-and-track-import
  [imp]
  (let [progress-chan (chan)]
    (go-loop [progress (<! progress-chan)]
      (when progress
        (imports/update (assoc imp :progress progress))
        (recur (<! progress-chan))))
    (import-data imp progress-chan)))

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
    (->> (range)
         (map (comp params
                    keyword
                    #(format "source-file-%s" %)))
         (take-while map?)
         (map #(images/find-or-create {:user-id (:id user)
                                       :content-type content-type
                                       :original-filename (:filename %)
                                       :body (read-bytes (:tempfile %))})))))

(defn- extract-import
  [{:keys [params authenticated]} images]
  (-> params
      (select-keys [:entity-name :options])
      (update-in-if [:options] #(json/parse-string % true))
      (assoc :user-id (:id authenticated)
             :image-ids (mapv :id images))))

(defn- step-2
  [req images]
  (let [imp (imports/create (extract-import req images))]
    (if (empty? (validation/error-messages imp))
      (let [{:keys [entity]} (launch-and-track-import imp)]
        (->response {:entity entity
                     :import imp}
                    201))
      (->response {:error (format "Unable to save the import record. %s"
                                  (->> (validation/error-messages imp)
                                       vals
                                       (string/join ", ")))}
                  422))))

(defn- step-1
  [{:keys [params authenticated] :as req}]
  (let [images (create-images params authenticated)]
    (if (not-any? #(validation/error-messages %) images)
      (step-2 req images)
      (->response {:error (format "Unable to save the source file(s). %s"
                                  (->> images
                                       (mapcat validation/error-messages)
                                       (mapcat vals)
                                       (string/join ", ")))}
                  422))))

(defn- create
  [req]
  (step-1 req))

(defn- find-and-authorize
  [{:keys [params authenticated]} action]
  (some-> params
          (select-keys [:id])
          (+scope ::models/import authenticated)
          imports/find-by
          (authorize action authenticated)))

(defn- show
  [req]
  (if-let [imp (find-and-authorize req ::authorization/show)]
    (->response imp)
    (not-found)))

(defn- index
  [{:keys [authenticated params]}]
  (->response (imports/search (-> params
                                  (select-keys [:entity-name])
                                  (+scope ::models/import authenticated)))))

(defn- delete
  [req]
  (if-let [imp (find-and-authorize req ::authorization/show)]
    (do
      (imports/delete imp)
      (->response))
    (not-found)))

(defn- start
  [{:keys [params authenticated]}]
  (let [imp (authorize (imports/find (:id params))
                       ::authorization/update
                       authenticated)]
    (launch-and-track-import imp)
    (->response imp)))

(defroutes routes
  (GET "/api/imports" req (index req))
  (POST "/api/imports" req (create req))
  (GET "/api/imports/:id" req (show req))
  (PATCH "/api/imports/:id" req (start req))
  (DELETE "/api/imports/:id" req (delete req)))
