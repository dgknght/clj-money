(ns clj-money.models.storage)

(defprotocol Storage
  "Provides data storage services for the application"
  (create-user
    [this user]
    "Creates a new user record")
  (select-users
    [this]
    "Returns all of the users in the system matching the specified criteria")
  (find-user-by-email
    [this email]
    "Returns the user having the specified email"))
