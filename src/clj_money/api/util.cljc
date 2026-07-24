(ns clj-money.api.util
  (:require [lambdaisland.uri :as uri]))

(defn +query
  [url & {:as query}]
  (uri/uri-str (apply uri/assoc-query url (mapcat identity query))))
