alter table users
  add column created_at timestamp with time zone not null default now(),
  add column updated_at timestamp with time zone not null default now();
alter table entities
  add column created_at timestamp with time zone not null default now(),
  add column updated_at timestamp with time zone not null default now();
alter table accounts
  add column created_at timestamp with time zone not null default now(),
  add column updated_at timestamp with time zone not null default now();
alter table transactions
  add column created_at timestamp with time zone not null default now(),
  add column updated_at timestamp with time zone not null default now();
alter table transaction_items
  add column created_at timestamp with time zone not null default now(),
  add column updated_at timestamp with time zone not null default now();
