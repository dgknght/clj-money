(ns clj-money.authorization.imports
  (:refer-clojure :exclude [update])
  (:require [clj-money.util :refer [model=]]
            [clj-money.authorization :as authorization]))

(defmethod authorization/allowed? [:import ::authorization/manage]
  [imp _ user]
  (model= user (:import/user imp)))

(defmethod authorization/scope :import
  [_ user]
  {:import/user user})
