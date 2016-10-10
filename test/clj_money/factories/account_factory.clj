(ns clj-money.factories.account-factory
  (:require [clojure.string :as s]
            [clj-factory.core :refer [deffactory defseq]]
            [faker.lorem :as lorem]))

(deffactory :account
  {:name (s/join " " (take 2 (lorem/words)))
   :type :asset})
