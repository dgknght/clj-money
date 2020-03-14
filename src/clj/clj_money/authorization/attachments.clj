(ns clj-money.authorization.attachments
  (:require [clj-money.models :as models]
            [clj-money.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [owner-or-granted?]]))

(defmethod authorization/allowed? [::models/attachment ::authorization/manage]
  [attachment action user]
  (owner-or-granted? attachment user action))
