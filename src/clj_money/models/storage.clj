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

  (select-accounts-by-entity-id
    [this entity-id]
    "Returns all accounts in the system"))
