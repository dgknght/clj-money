(ns clj-money.api.users
  (:require [dgknght.app-lib.api :as api]))

(defn authenticate
  [credentials success-fn error-fn]
  (api/post (api/path :users :authenticate)
            credentials
            success-fn
            error-fn))

(defn me
  [success-fn error-fn]
  (api/get (api/path :users :me)
           success-fn
           error-fn))
