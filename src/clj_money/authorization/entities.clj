(ns clj-money.authorization.entities
  (:refer-clojure :exclude [update])
  (:require [clj-money.authorization :as authorization]
            [clj-money.entities.auth-helpers :refer [owner-or-granted?]]))

(defmethod authorization/allowed? [:entity ::authorization/manage]
  [entity action user]
  (owner-or-granted? entity user action))

(defmethod authorization/scope :entity
  [_ user]
  {:entity/user user})
