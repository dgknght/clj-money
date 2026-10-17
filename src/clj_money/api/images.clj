(ns clj-money.api.images
  "Uploads an image (or a PDF) that isn't yet part of anything, e.g., a
  statement to attach to a reconciliation"
  (:require [dgknght.app-lib.api :as api]
            [clj-money.entities.images :as img]
            [clj-money.authorization.images]))

(defn- create
  [{{:keys [file]} :params
    :keys [authenticated]}]
  (if-let [image (img/find-or-create-upload file authenticated)]
    (api/creation-response image)
    (api/response {:message "A file is required."} 400)))

(def routes
  ["images" {:post {:handler create}}])
