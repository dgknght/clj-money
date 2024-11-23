(ns clj-money.authorization.attachments
  (:require [clj-money.db :as db]
            [clj-money.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [owner-or-granted?]]))

(defmethod authorization/allowed? [:attachment ::authorization/manage]
  [attachment action user]
  (owner-or-granted? attachment user action))

(defmethod authorization/scope :attachment
  [_ user]
  (db/model-type {:entity/user user}
                 :attachment))
