(ns clj-money.db.sql.grants
  (:require [clojure.walk :refer [keywordize-keys]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.db.sql :as sql]
            [clj-money.models.grants :as grants]))

(declare ->sql-refs)
(sql/def->sql-refs ->sql-refs :grant/entity :grant/user)

(declare ->model-refs)
(sql/def->model-refs ->model-refs :grant/entity :grant/user)

(defn- prepare-permissions
  [permissions]
  (reduce (fn [p k]
            (update-in-if p [k] #(set (map keyword %))))
          permissions
          grants/resource-types))

(defmethod sql/after-read :grant
  [grant]
  (-> grant
      (->model-refs)
      (update-in [:grant/permissions] (comp prepare-permissions
                                            sql/json->map))))

(defmethod sql/before-save :grant
  [grant]
  (-> grant
      (->sql-refs)
      (update-in [:grant/permissions] sql/->json)))
