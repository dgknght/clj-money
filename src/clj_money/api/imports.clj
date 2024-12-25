(ns clj-money.api.imports
  (:refer-clojure :exclude [update])
  (:require [clojure.string :as string]
            [clojure.pprint :refer [pprint]]
            [clojure.core.async :refer [go-loop <! chan]]
            [clojure.tools.logging :as log]
            [clojure.set :refer [rename-keys]]
            [cheshire.core :as json]
            [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.api :as api]
            [clj-money.util :as util]
            [clj-money.models.ref]
            [clj-money.db.sql.ref]
            [clj-money.io :refer [read-bytes]]
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
        (log/debugf "import progress for %s: %s" (:entity-name imp) progress)
        (models/put (assoc imp :import/progress progress))
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

(defn- ->source-file-key
  [index]
  (let [k (format "source-file-%s" index)]
    (some-fn (keyword "import" k)
             (keyword k))))

(defn- source-files
  [params]
  (->> (range)
       (map (comp #(% params)
                  ->source-file-key))
       (take-while map?)))

(defn- create-images
  [user source-files]
  (let [content-type (infer-content-type (first source-files))]
    (->> source-files
         (map #(images/find-or-create #:image{:user user
                                              :content-type content-type
                                              :original-filename (:filename %)
                                              :body (read-bytes (:tempfile %))}))
         (mapv util/->model-ref))))

(defn- extract-import
  [{:keys [params authenticated]}]
  (-> params
      (select-keys [:entity-name
                    :options
                    :import/entity-name
                    :import/options])
      (rename-keys {:entity-name :import/entity-name
                    :options :import/options})
      (update-in-if [:import/options] #(json/parse-string % true)); TODO: Why is this not parsed with the rest of the body?
      (assoc :import/user authenticated)))

(defn- step-2
  [req images]
  (let [imp (-> req
                extract-import
                (assoc :import/images images)
                models/put)
        {:keys [entity]} (launch-and-track-import imp)]
    (api/response {:entity entity
                   :import imp}
                  201)))

(defn- step-1
  [{:keys [params authenticated] :as req}]
  (step-2 req (create-images authenticated
                             (source-files params))))

(defn- create
  [req]
  (step-1 req))

(defn- find-and-authorize
  [{:keys [params authenticated]} action]
  (some-> params
          (select-keys [:id])
          (+scope :import authenticated)
          models/find-by
          (authorize action authenticated)))

(defn- show
  [req]
  (if-let [imp (find-and-authorize req ::authorization/show)]
    (api/response imp)
    api/not-found))

(defn- index
  [{:keys [authenticated params]}]
  (api/response (models/select (-> params
                                   (select-keys [:import/entity-name])
                                   (+scope :import authenticated)))))

(defn- delete
  [req]
  (if-let [imp (find-and-authorize req ::authorization/show)]
    (do
      (models/delete imp)
      (api/response))
    api/not-found))

(defn- start
  [{:keys [authenticated] :as req}]
  (let [imp (authorize (find-and-authorize req ::authorization/update)
                       ::authorization/update
                       authenticated)]
    (launch-and-track-import imp)
    (api/response imp)))

(def routes
  [["imports"
    ["" {:get {:handler index}
         :post {:handler create}}]
    ["/:id" {:get {:handler show}
             :patch {:handler start}
             :delete {:handler delete}}]]])
