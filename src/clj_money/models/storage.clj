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
  (update-account
    [this account]
    "Updates the specified account")
  (select-accounts-by-entity-id
    [this entity-id]
    "Returns all accounts in the system")
  (delete-account
    [this id]
    "Deletes the specified account")
  (find-accounts-by-name
    [this entity-id name]
    "Returns the account in the specified entity with the specified name")

  ; Transactions
  (create-transaction
    [this transaction]
    "Creates a new transaction record")
  (find-transaction-by-id
      [this id]
      "Returns the specified transaction")

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
  (find-transaction-item-by-index
    [this account-id index]
    "Returns the transaction item for the specified account having the specified index")
  (find-transaction-item-preceding-date
    [this account-id transaction-date]
    "Returns the transaction item with the highest index preceding the specifed date")
  (update-transaction-item
    [this transaction-item]
    "Updates the specified transaction item"))
