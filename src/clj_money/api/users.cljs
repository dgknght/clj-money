(ns clj-money.api.users
  (:require [dgknght.app-lib.web :refer [path]]
            [clj-money.api :as api :refer [add-error-handler]]))

(defn authenticate
  [credentials & {:as opts}]
  (api/post (path :oapi :users :authenticate)
            credentials
            (add-error-handler opts "Unable to authenticate the user: %s")))

(defn me
  [& {:as opts}]
  (api/get (api/path :users :me)
           {}
           (add-error-handler opts "Unable to retrieve your user profile: %s")))
