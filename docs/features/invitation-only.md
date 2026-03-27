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
  - user (the admin user that created the invitation)
- On the "Users" page, an administrator can see all invitations for the site.
- When an invitation is created, an email is sent to the recipient.
- When the email is sent, the invitation status is changed from :unsent to :sent.
- The email contains a link that the recipient can click to accept the invitation
  and create an account.
  - This link includes a secure token to ensure that only the recipient can
    accept the invitation.
  - On following this link, the user is presented with a form to fill out the
    fields required to create a `user` entity.
  - Known values, like `email` are prefilled from the invitation.
  - On successfully submitting the form and creating a `user` record, the status
    of the invitation is updated to `:accepted`.
  - After successful submission, the new user is redirected to the entities view
    to create an entity.
- The email contains a link that the recipient can click to decline the invitation.
  - This link uses the same secure token as the "accept" link.
  - On following this link:
    - the user is presented a "thank you" message for taking
      the time to respond.
    - the invitation status is updated to `:declined`
