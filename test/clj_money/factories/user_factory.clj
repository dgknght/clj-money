(ns clj-money.factories.user-factory
  (:require [clj-factory.core :refer [deffactory defseq]]
            [faker.name :as names]
            [faker.internet :as internet]))

(deffactory :user
  (let [first-name (names/first-name)]
  {:first-name first-name
   :last-name (names/last-name)
   :email (internet/email first-name)
   :password "please01"}))
