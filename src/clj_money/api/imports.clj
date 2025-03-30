(ns clj-money.api.imports
  (:refer-clojure :exclude [update])
  (:require [clojure.string :as string]
            [clojure.pprint :refer [pprint]]
            [clojure.core.async :as a]
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
            [clj-money.import :refer [import-data progress-xf]]
            [clj-money.import.gnucash]
            [clj-money.import.edn]
            [clj-money.authorization :refer [authorize +scope] :as authorization]
            [clj-money.authorization.imports]))

(defn- report-progress
  [imp progress-chan]
  (a/go-loop [progress (a/<! progress-chan)]
    (when progress
      (log/debugf "import progress for %s: %s" (:import/entity-name imp) progress)
      (models/put (assoc imp :import/progress progress))
      (recur (a/<! progress-chan)))))

(defn- launch-and-track-import
  [imp]
  (let [out-chan (a/chan 10 (progress-xf))]
    (report-progress imp out-chan)
    (let [{:keys [entity wait-chan]} (import-data imp
                                                  :out-chan out-chan)]
      (log/infof "import started for %s" (:import/entity-name imp))
      (a/go
        (a/<! wait-chan)
        (log/infof "import finished for %s" (:import/entity-name imp)))
      {:entity entity
       :import imp})))

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
                models/put)]
    (api/response (launch-and-track-import imp)
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
          (util/model-type :import)
          (+scope :import authenticated)
          models/find-by
          (authorize action authenticated)))

(defn- show
  [req]
  (or (some-> (find-and-authorize req ::authorization/show)
              api/response)
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
  [req]
  (or (some-> (find-and-authorize req ::authorization/update)
              launch-and-track-import
              api/response)
      api/not-found))

(def routes
  [["imports"
    ["" {:get {:handler index}
         :post {:handler create}}]
    ["/:id" {:get {:handler show}
             :patch {:handler start}
             :delete {:handler delete}}]]])
