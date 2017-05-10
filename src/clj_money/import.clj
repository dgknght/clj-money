(ns clj-money.import
  (:refer-clojure :exclude [update]))

(defmulti read-source
  (fn [_ source-type]
    source-type))

(defn import-data
  "Reads the contents from the specified input and saves
  the information using the specified storage. If an entity
  with the specified name is found, it is used, otherwise it
  is created"
  [storage-spec entity-name input source-type]

  )
