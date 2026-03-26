# Invitation Only

- A user can have one or more roles. The roles available are:
  - admin
  - user
- A user cannot have a role that is not one of the available roles.
- A user that has the role :admin has access to the "Users" menu item.
- A user that does not have the role :admin does not have access to the "Users"
  menu item.
- On the "Users" page (accessed by the "Users" menu item), an invitation can be
  created. An invitation includes:
  - recipient (an email address)
    - Required
    - Must be a well-formed email address
    - Must not be an email address already in the system
  - note (from the administrator to the invitee)
    - Optional
  - status (one of the following)
    - unsent
    - sent
    - accepted
    - declined
