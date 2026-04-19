(ns clj-money.config
  (:require [cljs.reader :as reader]))

(def env
  (some-> js/document
          (.getElementById "app-config")
          .-textContent
          reader/read-string))
