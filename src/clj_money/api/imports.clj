(ns clj-money.api.imports
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [clojure.string :as string]
            [clojure.core.async :refer [go]]
            [ring.util.response :refer [response]]
            [environ.core :refer [env]]
            [cemerick.friend :as friend]
            [clj-money.io :refer [read-bytes]]
            [clj-money.validation :as validation]
            [clj-money.models.images :as images]
            [clj-money.import :refer [import-data]]
            [clj-money.models.imports :as imports]))

(defn create
  [{params :params}]
  (let [user (friend/current-authentication)
        image (images/create (env :db) {:user-id (:id user)
                                        :original-filename (-> params :source-file :filename)
                                        :body (read-bytes (-> params :source-file :tempfile))})]
    (if (empty? (validation/error-messages image))
      (let [import (imports/create (env :db) {:user-id (:id user)
                                              :entity-name (:entity-name params)
                                              :image-id (:id image)})]
        (if (empty? (validation/error-messages import))
          (do
            (go (import-data (env :db) import))
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
  [req]
  (throw (ex-info "not implemented" {})))
