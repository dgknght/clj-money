(ns clj-money.web.lots
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [environ.core :refer [env]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [ring.util.response :refer :all]
            [clj-money.models.accounts :as accounts]
            [clj-money.models.commodities :as commodities]
            [clj-money.models.lots :as lots]
            [clj-money.web.shared :refer :all]
            [clj-money.web.money-shared :refer [budget-monitors]]))

(defmacro with-lots-layout
  [page-title entity-id options & content]
  `(with-layout
     ~page-title (assoc ~options :side-bar (budget-monitors (Integer. ~entity-id)))
     ~@content))

(defn index
  [{{:keys [account-id commodity-id]} :params}]
  (let [account (accounts/find-by-id (env :db) (Integer. account-id))
        commodity (commodities/find-by-id (env :db) (Integer. commodity-id))]
    (with-lots-layout (format "Lots of %s in %s" (:symbol commodity) (:name account)) (:entity-id account) {}
      "Lots go here")))
