(ns clj-money.models.storage)

(defprotocol Storage
  "Provides data storage services for the application"

  ; Users
  (create-user
    [this user]
    "Creates a new user record")
  (select-users
    [this]
    "Returns all of the users in the system matching the specified criteria")
  (find-user-by-email
    [this email]
    "Returns the user having the specified email")
  (user-exists-with-email?
    [this email]
    "Returns a boolean value indicating whether or not a user exists with
    the specified email")

  ; Entities
  (create-entity
    [this entity]
    "Creates a new entity record")
  (select-entities
    [this user-id]
    "Returns the entities belonging to the specified user")
  (entity-exists-with-name?
    [this user-id name]
    "Returns a boolean value indicating whether or not an entity exists for
    the specified user with the specified name")
  (find-entity-by-id
    [this id]
    "Returns the entity having the specified ID")
  (update-entity
    [this entity]
    "Updates the specified entity record")
  (delete-entity
    [this id]
    "Removes the entity from the data store")

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
  (select-accounts-by-entity-id
    [this entity-id]
    "Returns all accounts in the system")
  (delete-account
    [this id]
    "Deletes the specified account")
  (select-accounts-by-name
    [this entity-id name]
    "Returns the account in the specified entity with the specified name")

  ; Transactions
  (select-transactions-by-entity-id
    [this entity-id]
    "Returns transactions for the specified entity")
  (create-transaction
    [this transaction]
    "Creates a new transaction record")
  (find-transaction-by-id
    [this id]
    "Returns the specified transaction")
  (delete-transaction
    [this id]
    "Deletes the specified transaction record")
  (update-transaction
    [this id]
    "Updates the specified transaction")

  ; Transaction items
  (create-transaction-item
    [this transaction-item]
    "Creates a new transaction item record")
  (select-transaction-items-by-transaction-id
    [this transaction-id]
    "Returns the transaction items belonging to the specified transaction")
  (select-transaction-items-by-account-id
    [this account-id]
    "Returns the transaction items belonging to the specified account")
  (select-transaction-items-by-account-id-and-starting-index
    [this account-id index]
    "Returns the transaction items for the specified account having an index greater than or equal to the specified index")
  (select-transaction-items-by-account-id-on-or-after-date
    [this account-id transaction-date]
    "Returns the transaction items for the specified account ocurring on or after the specified date")
  (find-transaction-item-by-id
    [this id]
    "Returns the transaction item having the specified id")
  (select-transaction-items-preceding-date
    [this account-id transaction-date]
    "Returns the transaction items preceding the specifed date in descending order by seqence")
  (find-last-transaction-item-on-or-before
    [this account-id transaction-date]
    "Returns the last transaction item that is on or before the specified date")
  (update-transaction-item
    [this transaction-item]
    "Updates the specified transaction item")
  (update-transaction-item-index-and-balance
    [this transaction-item]
    "Updates the specified transaction item, index and balance fields only, returns true if the values changes, false if not")
  (delete-transaction-item
    [this id]
    "Deletes the specified transaction item record")
  (delete-transaction-items-by-transaction-id
    [this transaction-id]
    "Deletes the transaction items having the specified id")

  ; Data integrity transactions
  (with-transaction
    [this func]
    "Executes the specified function which expects a single argument,
    which is a transacted storage instance"))
