(ns clj-money.models.storage
  (:refer-clojure :exclude [update]))

(defprotocol Storage
  "Provides data storage services for the application"

  (create
    [this model]
    "Saves a new model in the data store")
  (select
    [this criteria options]
    "Returns a list of models matching the criteria")
  (update
    [this model]
    [this attr criteria]
    "Updates the specified model with 2 arguments, or applies the specified
    attributes to records matching the specified criteria with 3 arguments")
  (delete
    [this model]
    "Removes the model from the data store")

  ; Data integrity transactions
  (with-transaction
    [this func]
    "Executes the specified function which expects a single argument,
    which is a transacted storage instance"))
