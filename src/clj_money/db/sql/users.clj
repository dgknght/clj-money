(ns clj-money.db.sql.users
  (:require [clojure.pprint :refer [pprint]]
            [java-time.api :as t]
            [stowaway.criteria :as criteria]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.util :as util]
            [clj-money.db.sql :as sql]))

(defmulti ^:private coerce-timestamp type)

(defmethod coerce-timestamp ::util/vector
  [[oper & vs]]
  (apply vector oper (map coerce-timestamp vs)))

(defmethod coerce-timestamp :default
  [v]
  (t/instant->sql-timestamp v))

(defn- coerce-types
  [user]
  (update-in-if user [:user/token-expires-at] coerce-timestamp))

(defmethod sql/before-save :user
  [user]
  (-> user
      coerce-types
      (dissoc :user/password-confirmation)))

(defmethod sql/prepare-criteria :user
  [criteria]
  (criteria/apply-to criteria coerce-types))
