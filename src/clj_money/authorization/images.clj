(ns clj-money.authorization.images
  (:refer-clojure :exclude [update])
  (:require [clj-money.util :refer [entity=] :as util]
            [clj-money.entities :as entities]
            [clj-money.authorization :as authorization]
            [clj-money.entities.auth-helpers :refer [user-granted-access?]]))

(defmethod authorization/allowed? [:image ::authorization/manage]
  [image action user]
  (or (entity= user (:image/user image))
      (let [entity (entities/find-by
                     (util/entity-type
                       {:attachment/image image}
                       :entity))]
        (user-granted-access? image entity user action))))

(defmethod authorization/scope :image
  [_ user]
  {:image/user user})
