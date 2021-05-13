(ns clj-money.authorization.entities
  (:refer-clojure :exclude [update])
  (:require [clj-money.models :as models]
            [dgknght.app-lib.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [owner-or-granted?]]))

(defmethod authorization/allowed? [::models/entity ::authorization/manage]
  [entity action user]
  (owner-or-granted? entity user action))

(defmethod authorization/scope ::models/entity
  [_ user]
  {:user-id (:id user)})
