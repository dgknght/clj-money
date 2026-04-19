(ns clj-money.api.config
  (:require [dgknght.app-lib.api :as api]
            [clj-money.config :refer [env]]))

(defn- config [_req]
  (api/response {:oauth-providers (vec (or (env :oauth-providers) #{}))}))

(def unauthenticated-routes
  ["config" {:get {:handler config}}])
