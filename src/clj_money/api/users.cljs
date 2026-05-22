(ns clj-money.api.users
  (:require [dgknght.app-lib.web :refer [path]]
            [clj-money.api :as api :refer [add-error-handler]]))

(defn select
  [& {:as opts}]
  (api/get (api/path :users)
           {}
           (add-error-handler opts "Unable to retrieve the users: %s")))

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

(defn any-users?
  [& {:as opts}]
  (api/get (path :oapi :users :any)
           {}
           (add-error-handler opts "Unable to check if any users exist: %s")))

(defn create-admin
  [user & {:as opts}]
  (api/post (path :oapi :users :admin)
            user
            (add-error-handler opts "Unable to create the admin user: %s")))
