alter table users
  add column password_reset_token char(32),
  add column token_expires_at bigint;

create index uk_users_password_reset_token on users (password_reset_token);
