(ns clj-money.models
  "Backward compatibility namespace - models have been renamed to entities"
  (:require [clj-money.entities]))

(require '[clj-money.entities :as models])
(doseq [[k v] (ns-publics 'clj-money.entities)]
  (intern 'clj-money.models k v))
