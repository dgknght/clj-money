(ns clj-money.factories.entity-factory
  (:require [clojure.string :as s]
            [clj-factory.core :refer [deffactory]]
            [faker.lorem :as lorem]))

(deffactory :entity
  {:name (s/join " " (take 2 (lorem/words)))})
