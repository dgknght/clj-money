(ns clj-money.web.money-shared
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [rename-keys]]
            [environ.core :refer [env]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [ring.util.response :refer :all]
            [clj-money.models.accounts :as accounts])
  (:use [clj-money.web.shared :refer :all]))

(defn account-options
  "Returns a list of account for the specified entity as options ready for
  use with 'select-field'"
  ([entity-id] (account-options entity-id {}))
  ([entity-id options]
   (cond->> entity-id
     true (accounts/select-by-entity-id (env :db))
     (contains? options :types) (filter #(contains? (:types options) (:type %)))
     true (sort-by :name)
     true (map #(select-keys % [:id :name]))
     true (map #(rename-keys % {:id :value
                                :name :caption}))
     (contains? options :include-none?) (concat [{:value "" :caption "None"}]))))
