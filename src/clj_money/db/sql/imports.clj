(ns clj-money.db.sql.imports
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.db.sql :as sql]
            [clj-money.db.sql.types :refer [->json json->map]]))

(defmethod sql/before-save :import
  [imp]
  (-> imp
      (rename-keys {:import/images :import/image-ids})
      (update-in-if [:import/image-ids] (comp #(into-array Long/TYPE %)
                                              #(mapv :id %)))
      (update-in-if [:import/options] ->json)
      (update-in-if [:import/progress] ->json)))

(defmethod sql/after-read :import
  [imp]
  (-> imp
      (rename-keys {:import/image-ids :import/images})
      (update-in [:import/images] #(mapv (partial hash-map :id) %))
      (update-in [:import/options] json->map)
      (update-in-if [:import/progress] json->map)))
