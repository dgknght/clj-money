(ns clj-money.models.storage)

(defprotocol Storage
  "Provides data storage services for the application"

  ; Users
  (create-user
    [this user]
    "Creates a new user record")
  (select-users
    [this]
    [this criteria]
    [this criteria options]
    "Returns all of the users in the system matching the specified criteria")
  (update-user
    [this user]
    "Updates a new user record")

  ; Entities
  (create-entity
    [this entity]
    "Creates a new entity record")
  (select-entities
    [this user-id options]
    "Returns the entities belonging to the specified user")
  (find-entity-by-id
    [this id]
    "Returns the entity having the specified ID")
  (update-entity
    [this entity]
    "Updates the specified entity record")
  (delete-entity
    [this id]
    "Removes the entity from the data store")

  ; Grants
  (create-grant
    [this grant]
    "Creates a new grant record")
  (update-grant
    [this grant]
    "Updates the specified grant")
  (delete-grant
    [this id]
    "Deletes the specified grant")
  (select-grants
    [this criteria]
    [this criteria options]
    "Returns the grants matching the specified criteria")

  ; Accounts
  (create-account
    [this account]
    "Creates a new account record")
  (find-account-by-id
    [this id]
    "Returns the account having the specified ID")
  (find-account-by-entity-id-and-name
    [this entity-id account-name]
    "Returns the account having the specified entity-id and name")
  (update-account
    [this account]
    "Updates the specified account")
  (delete-account
    [this id]
    "Deletes the specified account")
  (select-accounts
    [this criteria]
    "Returns the accounts matching the specified criteria")

  ; Commodities
  (create-commodity
    [this commodity]
    "Creates a new commodity record")

  (find-commodity-by-id
    [this id]
    "Returns the specified commodity record")

  (update-commodity
    [this commodity]
    "Updates the specified commodity")

  (select-commodities
    [this criteria]
    [this criteria options]
    "Returns a list of commodities matching the specified criteria")

  (delete-commodity
    [this id]
    "Deletes the specified commodity record")

  ; Prices
  (create-price
    [this price]
    "Creates a new commodity price record")

  (select-prices
    [this criteria]
    [this criteria options]
    "Returns a list of prices matching the specified criteria")

  (find-price-by-id
    [this id]
    "Returns the price having the specified ID")

  (update-price
    [this price]
    "Updates the specified price record")

  (delete-price
    [this id]
    "Deletes the specified price record")

  (delete-prices-by-commodity-id
    [this commodity-id]
    "Deletes all prices for the specified commodity")

  ; Lots
  (create-lot
    [this lot]
    "Creates a new lot record")

  (select-lots-by-entity-id
    [this entity-id]
    "Returns the lots for the specified entity")

  (select-lots-by-commodity-id
    [this commodity-id]
    "Returns the lots for the specified commodity")

  (select-lots-by-transaction-id
    [this transaction-id]
    "Returns the lots for the specified transaction")

  (update-lot
    [this lot]
    "Updates the specified lot record")

  (find-lot-by-id
    [this id]
    "Returns the lot having the specified id")

  (select-lots
    [this criteria]
    "Returns the lots matching the specified criteria")

  (delete-lot
    [this lot-id]
    "Deletes the specified lot record")

  (create-lot->transaction-link
    [this link]
    "Creates a record linking a lot to a transaction")

  (delete-lot->transaction-link
    [this lot-id transaction-id]
    "Removes a record linking a lot to a transaction")

  (select-lots-transactions-by-transaction-id
    [this transaction-id]
    "Selects lots-transactions record by transaction-id")

  ; Transactions
  (select-transactions
    [this criteria options]
    "Returns transactions matching the specified criteria")

  (create-transaction
    [this transaction]
    "Creates a new transaction record")
  (delete-transaction
    [this id transaction-date]
    "Deletes the specified transaction record")
  (update-transaction
    [this id]
    "Updates the specified transaction")

  ; Transaction items
  (create-transaction-item
    [this transaction-item]
    "Creates a new transaction item record")
  (update-transaction-item
    [this transaction-item]
    "Updates the specified transaction item")
  (update-transaction-item-index-and-balance
    [this transaction-item]
    "Updates the specified transaction item, index and balance fields only, returns true if the values changes, false if not")
  (delete-transaction-item
    [this id transaction-date]
    "Deletes the specified transaction item record")
  (delete-transaction-items-by-transaction-id
    [this transaction-id transaction-date]
    "Deletes the transaction items having the specified id")
  (set-transaction-item-reconciled
    [this reconciliation-id transaction-item-id transaction-date]
    "Updates the specified transaction items to indicate they belong to a reconciliation")
  (unreconcile-transaction-items-by-reconciliation-id
    [this reconciliation-id date-range]
    "Unsets the reconciliation ID to null for all matching values")
  (select-transaction-items
    [this criteria]
    [this criteria options]
    "Returns a list of transaction items matching the criteria")

  ; Reconciliations
  (create-reconciliation
    [this reconciliation]
    "Creates a new reconciliation record")
  (select-reconciliations
    [this criteria]
    [this criteria options]
    "Returns reconciliation records for the specified account")
  (update-reconciliation
    [this reconciliation]
    "Updates the specified reconciliation")
  (delete-reconciliation
    [this id]
    "Removes the reconciliation from the system")

  ; Budgets
  (create-budget
    [this budget]
    "Creates a new budget record")

  (find-budget-by-date
    [this entity-id date]
    "Returns the budget containing the specified date")

  (select-budgets
    [this criteria]
    [this criteria options]
    "Returns budgets for the specified criteria")

  (update-budget
    [this budget]
    "Updates the specified budget")

  (delete-budget
    [this id]
    "Deletes the specified budget")

  ; Budget items
  (create-budget-item
    [this budget-item]
    "Creates a new budget item")

  (update-budget-item
    [this budget-item]
    "Updates an existing budate item")

  (find-budget-item-by-id
    [this id]
    "Returns the budget item having the specified ID")

  (select-budget-items-by-budget-id
    [this budget-id]
    "Returns the budget items for the specified budget")

  (delete-budget-item
    [this id]
    "Deletes the specified budget item")

  ; Images
  (create-image
    [this image]
    "Creates a new image record")

  (select-images
    [this criteria]
    [this criteria options]
    "Selects images matching the specified criteria")

  (find-image-by-id
    [this id]
    "Returns the specified image record")

  (delete-image
    [this id]
    "Deletes the specified image record")

  ; Attachments
  (create-attachment
    [this attachment]
    "Creates a new attachment record")

  (select-attachments
    [this criteria]
    [this criteria options]
    "Returns matching attachments")

  (delete-attachment
    [this id]
    "Deletes the specified attachment record")

  ; Imports
  (create-import
    [this import]
    "Creates a new import record")

  (find-import-by-id
    [this id]
    "Returns the import having the specified id")

  (update-import
    [this import]
    "Updates an existing import record")

  ; Settings
  (put-setting
    [this setting-name setting-value]
    "Writes an application setting to the database")

  (get-setting
    [this setting-name]
    [this setting-name transform-fn]
    "Reads an application setting from the database")

  ; Data integrity transactions
  (with-transaction
    [this func]
    "Executes the specified function which expects a single argument,
    which is a transacted storage instance"))
