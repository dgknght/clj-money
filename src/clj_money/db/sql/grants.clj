(ns clj-money.db.sql.grants
  (:require [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.db.sql :as sql]
            [clj-money.models.grants :as grants]))

(defn- prepare-permissions
  [permissions]
  (reduce (fn [p k]
            (update-in-if p [k] #(set (map keyword %))))
          permissions
          grants/resource-types))

(defmethod sql/after-read :grant
  [grant]
  (update-in grant [:grant/permissions] (comp prepare-permissions
                                              sql/json->map)))

(defmethod sql/before-save :grant
  [grant]
  (update-in grant [:grant/permissions] sql/->json))
