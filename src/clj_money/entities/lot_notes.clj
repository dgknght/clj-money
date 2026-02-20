(ns clj-money.entities.lot-notes
  (:require [clojure.spec.alpha :as s]
            [java-time.api :as t]
            [clj-money.entities :as entities]))

(s/def :lot-note/lot ::entities/entity-ref)
(s/def :lot-note/transaction-date t/local-date?)
(s/def :lot-note/memo string?)
^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(s/def ::entities/lot-note
  (s/keys :req [:lot-note/lot
                :lot-note/transaction-date
                :lot-note/memo]))
