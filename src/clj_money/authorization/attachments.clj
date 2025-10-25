(ns clj-money.authorization.attachments
  (:require [clj-money.util :as util]
            [clj-money.authorization :as authorization]
            [clj-money.entities.auth-helpers :refer [owner-or-granted?]]))

(defmethod authorization/allowed? [:attachment ::authorization/manage]
  [attachment action user]
  (owner-or-granted? attachment user action))

(defmethod authorization/scope :attachment
  [_ user]
  (util/model-type {:entity/user user}
                 :attachment))
