(ns clj-money.authorization.images
  (:refer-clojure :exclude [update])
  (:require [clj-money.util :refer [model=]]
            [clj-money.db :as db]
            [clj-money.models :as models]
            [clj-money.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [user-granted-access?]]))

(defmethod authorization/allowed? [:image ::authorization/manage]
  [image action user]
  (or (model= user (:image/user image))
      (let [entity (models/find-by
                     (db/model-type
                       {:attachment/image image}
                       :entity))]
        (user-granted-access? image entity user action))))

(defmethod authorization/scope :image
  [_ user]
  {:image/user user})
