(ns clj-money.web.grants
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [environ.core :refer [env]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [ring.util.response :refer :all]
            [ring.util.codec :refer [url-encode]]
            [clj-money.authorization :refer [authorize
                                             apply-scope
                                             tag-resource]]
            [clj-money.pagination :as pagination]
            [clj-money.validation :as validation]
            [clj-money.models.entities :as entities])
  (:use [clj-money.web.shared :refer :all]))

(defn index
  [{{entity :entity} :params}]
  (with-layout (format "User grants for entity %s" (:name entity)) {}
    "List goes here"))

(defn new-grant
  [req]
  "New")

(defn create
  [req]
  "Create")

(defn show
  [req]
  "Show")

(defn edit
  [req]
  "Edit")

(defn update
  [req]
  "Update")

(defn delete
  [req]
  "Delete")
