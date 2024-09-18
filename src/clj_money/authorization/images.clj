(ns clj-money.authorization.images
  (:refer-clojure :exclude [update])
  (:require [clj-money.models :as models]
            [dgknght.app-lib.authorization :as authorization]
            [clj-money.models.auth-helpers :refer [owner-or-granted?]]))

(defmethod authorization/allowed? [::models/image ::authorization/manage]
  [image action user]
  (owner-or-granted? image user action))
