alter table budgets
  drop column start_date,
  add column start_date date not null,
  drop column end_date,
  add column end_date date not null;
alter table lots
  drop column purchase_date,
  add column purchase_date date not null;
alter table prices
  drop column trade_date,
  add column trade_date date not null;
alter table reconciliations
  drop column end_of_period,
  add column end_of_period date not null;
alter table transactions
  drop column transaction_date,
  add column transaction_date date not null;
alter table users
  drop column token_expires_at,
  add column token_expires_at timestamp;
