(ns clj-money.api.images
  (:require [clj-money.api :as api :refer [add-error-handler]]))

(defn create
  "Uploads a file, given as a js/File, that isn't yet part of anything, e.g.,
  a statement to attach to a reconciliation. The response is the image."
  [file & {:as opts}]
  (api/post (api/path :images)
            {:file [file (.-name file)]}
            (-> opts
                ; the encoding would otherwise set the Accept header to
                ; application/multipart
                (assoc :encoding :multipart
                       :accept "application/edn")
                (add-error-handler "Unable to upload the file: %s"))))
