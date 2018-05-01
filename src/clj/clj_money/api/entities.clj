(ns clj-money.api.entities
  (:refer-clojure :exclude [update])
  (:require [ring.util.response :refer [response]]
            [cemerick.friend :as friend]
            [clj-money.validation :as validation]
            [clj-money.models.entities :as entities]))

(defn index
  [req]
  (response {:entities "This is a test"}))
