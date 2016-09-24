(ns clj-money.factories.entity-factory
  (:require [clj-factory.core :refer [deffactory defseq]]
            [faker.lorem :as lorem]))

(deffactory :entity
  {:name (lorem/words)})
