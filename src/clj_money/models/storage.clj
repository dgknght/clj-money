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

  ; Accounts
  (create-account
    [this account]
    "Creates a new account record")

  (select-accounts
    [this]
    "Returns all accounts in the system"))
