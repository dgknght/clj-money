(ns clj-money.db.datomic.imports
  (:require [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.util :as util]
            [clj-money.db.datomic :as datomic]))

(defmethod datomic/before-save :import
  [imp]
  (update-in-if imp [:import/options] util/qualify-keys :import-option))
