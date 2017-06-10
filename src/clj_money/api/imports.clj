(ns clj-money.api.imports
  (:refer-clojure :exclude [update])
  (:require [clojure.pprint :refer [pprint]]
            [clojure.tools.logging :as log]
            [ring.util.response :refer [response]]
            [environ.core :refer [env]]
            [cemerick.friend :as friend]
            [clj-money.io :refer [read-bytes]]
            [clj-money.validation :as validation]
            [clj-money.models.images :as images]
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
          (response {:import import})
          (response {:errors (validation/error-messages import)})))
      (response {:errors (validation/error-messages image)}))))

(defn show
  [req]
  (throw (ex-info "not implemented" {})))
