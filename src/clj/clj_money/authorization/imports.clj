(ns clj-money.authorization.imports
  (:refer-clojure :exclude [update])
  (:require [clj-money.models :as models]
            [clj-money.authorization :as authorization]))

(defmethod authorization/allowed? [::models/import ::authorization/manage]
  [imp _ user]
  (= (:id user) (:user-id imp)))

(defmethod authorization/scope ::models/import
  [_ user]
  {:user-id (:id user)})
