(ns clj-money.api.users
  (:require [dgknght.app-lib.web :refer [path]]
            [clj-money.api :as api :refer [handle-ex]]))

(defn authenticate
  [credentials xf]
  (api/post (path :oapi :users :authenticate)
            credentials
            {:transform xf
             :handle-ex (handle-ex "Unable to authenticate the user: %s")}))

(defn me
  [xf]
  (api/get (api/path :users :me)
           {}
           {:transform xf
            :handle-ex (handle-ex "Unable to retrieve your user profile: %s")}))
