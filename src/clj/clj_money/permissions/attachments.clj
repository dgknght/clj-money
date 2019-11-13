(ns clj-money.permissions.attachments
  (:refer-clojure :exclude [update])
  (:require [clj-money.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [user-owns-entity?
                                                   user-granted-access?]]))

(authorization/allow :attachment user-owns-entity?)
(authorization/allow :attachment user-granted-access?)

; TODO Need a better way to set scope on models
; that don't map directly to an entity
#_(authorization/set-scope :attachment {:entity-id this-could-get-ugly})
