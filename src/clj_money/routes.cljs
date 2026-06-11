(ns clj-money.routes
  (:require [cljs.pprint :refer [pprint]]))

(def ^:private server-pattern
  #"^/(?:api/|oapi/|auth/|app/)")

(def server-path?
  (comp boolean
        (partial re-find server-pattern)))

(def ^:private public-pattern
  #"^/(?:login|setup|accept-invitation|decline-invitation)")

(def public-path?
  (comp boolean
        (partial re-find public-pattern)))
