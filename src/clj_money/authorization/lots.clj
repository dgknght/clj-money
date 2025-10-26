(ns clj-money.authorization.lots
  (:refer-clojure :exclude [update])
  (:require [clj-money.authorization :as authorization]
            [clj-money.entities.auth-helpers :refer [owner-or-granted?]]))

(defmethod authorization/allowed? [:lot ::authorization/manage]
  [lot action user]
  (owner-or-granted? lot user action))

(defmethod authorization/scope :lot
  [_ user]
  {:entity/user user})
