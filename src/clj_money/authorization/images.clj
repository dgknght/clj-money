(ns clj-money.authorization.images
  (:require [clj-money.util :refer [entity=] :as util]
            [clj-money.entities :as entities]
            [clj-money.authorization :as authorization]
            [clj-money.entities.auth-helpers :refer [user-granted-access?]]))

; An image is shared with an entity by what refers to it, and a user granted
; access to that kind of thing in the entity may see the image. (There is no
; grant for images themselves.)
(def ^:private sharing-refs
  {:attachment/image :attachment
   :reconciliation/statement :reconciliation})

(defn- granted-through?
  [image user action [attr resource-type]]
  (when-let [entity (entities/find-by
                      (util/entity-type
                        {attr image}
                        :entity))]
    (user-granted-access? (util/entity-type {} resource-type)
                          entity
                          user
                          action)))

(defmethod authorization/allowed? [:image ::authorization/manage]
  [image action user]
  (or (entity= user (:image/user image))
      (some (partial granted-through? image user action)
            sharing-refs)))

(defmethod authorization/scope :image
  [_ user _action]
  {:image/user user})
