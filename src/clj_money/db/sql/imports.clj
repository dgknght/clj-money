(ns clj-money.db.sql.imports
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.db.sql :as sql]))

(declare ->sql-refs)
(sql/def->sql-refs ->sql-refs :import/user)

(declare ->model-refs)
(sql/def->model-refs ->model-refs :import/user)

(defmethod sql/before-save :import
  [imp]
  (-> imp
      (rename-keys {:import/images :import/image-ids})
      (update-in-if [:import/image-ids] (comp #(into-array Long/TYPE %)
                                           #(mapv :id %)))
      (update-in-if [:import/options] sql/->json)
      (update-in-if [:import/progress] sql/->json)
      ->sql-refs))

(defmethod sql/after-read :import
  [imp]
  (-> imp
      (rename-keys {:import/image-ids :import/images})
      (update-in [:import/images] #(mapv (partial hash-map :id) %))
      (update-in [:import/options] sql/json->map)
      (update-in-if [:import/progress] sql/json->map)
      ->model-refs))
