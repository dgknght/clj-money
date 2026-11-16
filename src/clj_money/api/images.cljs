(ns clj-money.api.images
  (:require [clj-money.api :as api :refer [add-error-handler]]))

(defn create
  "Uploads a file, given as a js/File, that isn't yet part of anything, e.g.,
  a statement to attach to a reconciliation. The response is the image."
  [file & {:as opts}]
  (api/post-multipart (api/path :images)
                      {:file [file (.-name file)]}
                      (add-error-handler opts "Unable to upload the file: %s")))
