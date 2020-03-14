(ns clj-money.authorization.grants
  (:refer-clojure :exclude [update])
  (:require [clj-money.models :as models]
            [clj-money.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [owner-or-granted?]]))

(defmethod authorization/allowed? [::models/grant ::authorization/manage]
  [grant action user]
  (owner-or-granted? grant user action))
