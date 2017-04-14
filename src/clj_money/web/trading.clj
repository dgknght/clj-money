(ns clj-money.web.trading
  (:refer-clojure :exclude [update])
  (:require [clojure.tools.logging :as log]
            [environ.core :refer [env]]
            [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [ring.util.response :refer :all]
            [clj-money.trading :as trading]))

(defn new-purchase
  [req]
  "new-purchase"
  )

(defn purchase
  [req]
  "purchase")

(defn new-sale
  [req]
  "new-sale")

(defn sell
  [req]
  "sell")
