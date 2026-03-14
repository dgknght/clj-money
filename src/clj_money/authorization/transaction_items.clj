(ns clj-money.authorization.transaction-items
  (:require [clj-money.util :as util]
            [clj-money.authorization :as authorization]))

(defmethod authorization/scope :transaction-item
  [_ user]
  (util/entity-type {:entity/user user}
                    :transaction-item))
