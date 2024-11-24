(ns clj-money.db.sql.identities
  (:require [clojure.pprint :refer [pprint]]
            [dgknght.app-lib.core :refer [update-in-if]]
            [clj-money.db.sql :as sql]))

(defn- coerce
  [ident]
  (update-in-if ident [:identity/provider] name))

(defmethod sql/before-save :identity
  [ident]
  (coerce ident))

(defmethod sql/after-read :identity
  [ident]
  (update-in ident [:identity/provider] keyword))
