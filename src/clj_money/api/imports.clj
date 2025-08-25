(ns clj-money.api.imports
  (:refer-clojure :exclude [update])
  (:require [clojure.string :as string]
            [clojure.pprint :refer [pprint]]
            [clojure.core.async :as a]
            [clojure.tools.logging :as log]
            [clojure.set :refer [rename-keys]]
            [clojure.edn :as edn]
            [dgknght.app-lib.core :refer [update-in-if]]
            [dgknght.app-lib.api :as api]
            [clj-money.util :as util]
            [clj-money.progress :as prog]
            [clj-money.progress.redis]
            [clj-money.models.ref]
            [clj-money.db.ref]
            [clj-money.images.sql]
            [clj-money.io :refer [read-bytes]]
            [clj-money.models :as models]
            [clj-money.models.images :as images]
            [clj-money.import :refer [import-data progress-xf]]
            [clj-money.import.gnucash]
            [clj-money.import.edn]
            [clj-money.authorization :refer [authorize +scope] :as authorization]
            [clj-money.authorization.imports]))

(defn- launch-and-track
  [imp]
  (let [out-chan (a/chan (a/sliding-buffer 10)
                         (progress-xf imp))
        {:keys [entity wait-chan]} (import-data imp
                                                :out-chan out-chan)]
    (log/infof "[import] started for %s" (:import/entity-name imp))
    (a/go
      (a/<! wait-chan)
      (log/infof "[import] finished for %s" (:import/entity-name imp)))
    {:entity entity
     :import imp}))

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

(defn- ->image
  "Takes an uploaded file and converts it into an image map"
  [user content-type]
  (fn [img]
    (-> img
        (rename-keys {:filename :image/original-filename
                      :tempfile :image/content})
        (update-in [:image/content] read-bytes)
        (merge #:image{:user user
                       :content-type content-type})
        (select-keys [:image/user
                      :image/content-type
                      :image/content
                      :image/original-filename]))))

(defn- create-images
  [user source-files]
  (let [content-type (infer-content-type (first source-files))]
    (mapv (comp util/->model-ref
                images/find-or-create
                (->image user content-type))
          source-files)))

(defn- extract-import
  [{:keys [params authenticated]}]
  (-> params
      (select-keys [:entity-name
                    :options
                    :import/entity-name
                    :import/options])
      (rename-keys {:entity-name :import/entity-name
                    :options :import/options})
      (update-in-if [:import/options] edn/read-string) ; on create, the data is posted in a multipart from request, not edn.
      (assoc :import/user authenticated)))

(defn- create
  [{:keys [params authenticated] :as req}]
  (-> req
      extract-import
      (assoc :import/images
             (create-images authenticated
                            (source-files params)))
      models/put
      launch-and-track
      (api/response 201)))

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

(defn- fetch-progress
  [{:keys [id]}]
  (let [tracker (prog/tracker id)]
    (prog/get tracker)))

(defn- progress
  [req]
  (or (some-> (find-and-authorize req ::authorization/show)
              fetch-progress
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
              launch-and-track
              api/response)
      api/not-found))

(def routes
  [["imports"
    ["" {:get {:handler index}
         :post {:handler create}}]
    ["/:id"
     ["" {:get {:handler show}
          :patch {:handler start}
          :delete {:handler delete}}]
     ["/progress" {:get {:handler progress}}]]]])
