(ns clj-money.db.datomic.imports
  (:require [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.util :as util]
            [clj-money.db.datomic :as datomic]))

(defmethod datomic/before-save :import
  [imp]
  (-> imp
      (update-in-if [:import/progress] pr-str)
      (update-in-if [:import/options] util/qualify-keys :import-option)))

(defmethod datomic/after-read :import
  [imp]
  (-> imp
      (update-in-if [:import/progress] read-string)
      (update-in-if [:import/options] update-keys (comp keyword name))))
