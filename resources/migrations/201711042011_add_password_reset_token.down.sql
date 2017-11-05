alter table users
  drop column password_reset_token,
  drop column token_expires_at;
