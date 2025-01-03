(ns clj-money.authorization.images
  (:refer-clojure :exclude [update])
  (:require [clj-money.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [owner-or-granted?]]))

(defmethod authorization/allowed? [:image ::authorization/manage]
  [image action user]
  (owner-or-granted? image user action))

(defmethod authorization/scope :image
  [_ user]
  {:image/user user})
