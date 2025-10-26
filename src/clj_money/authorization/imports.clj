(ns clj-money.authorization.imports
  (:refer-clojure :exclude [update])
  (:require [clj-money.util :refer [entity=]]
            [clj-money.authorization :as authorization]))

(defmethod authorization/allowed? [:import ::authorization/manage]
  [imp _ user]
  (entity= user (:import/user imp)))

(defmethod authorization/scope :import
  [_ user]
  {:import/user user})
