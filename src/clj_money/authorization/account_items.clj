(ns clj-money.authorization.account-items
  (:require [clj-money.util :as util]
            [clj-money.authorization :as authorization]))

(defmethod authorization/scope :account-item
  [_ user]
  (util/entity-type {:entity/user user}
                    :account-item))
