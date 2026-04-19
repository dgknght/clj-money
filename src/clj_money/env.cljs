(ns clj-money.env
  (:require [cljs.reader :as reader]))

(def env
  (some-> js/document
          (.getElementById "app-config")
          .-textContent
          reader/read-string))
