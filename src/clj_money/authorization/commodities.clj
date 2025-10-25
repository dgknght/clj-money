(ns clj-money.authorization.commodities
  (:refer-clojure :exclude [update])
  (:require [clj-money.authorization :as authorization]
            [clj-money.util :as util]
            [clj-money.entities.auth-helpers :refer [owner-or-granted?]]))

(defmethod authorization/allowed? [:commodity ::authorization/manage]
  [commodity action user]
  (owner-or-granted? commodity user action))

(defmethod authorization/scope :commodity
  [_ user]
  (util/model-type {:entity/user user} :commodity))
