create extension if not exists pgcrypto;

create table transactions_base (
  id uuid not null primary key default gen_random_uuid(),
  transaction_date date not null,
  entity_id int not null,
  description varchar(200) not null,
  memo varchar(200),
  created_at timestamp with time zone not null default now(),
  updated_at timestamp with time zone not null default now(),
  foreign key (entity_id) references entities (id) on delete cascade
);

create table transaction_items_base (
  id uuid not null primary key default gen_random_uuid(),
  transaction_id uuid not null,
  transaction_date date not null,
  account_id int not null,
  action varchar(10) not null,
  amount numeric(10, 2),
  value numeric(10, 2),
  balance numeric(10, 2),
  memo varchar(200),
  index bigint not null,
  created_at timestamp with time zone not null default now(),
  updated_at timestamp with time zone not null default now(),
  foreign key (account_id) references accounts (id) on delete cascade
);

alter table lots_transactions drop column transaction_id;
alter table lots_transactions add column transaction_id uuid not null;

alter table attachments drop column transaction_id;
alter table attachments add column transaction_id uuid not null;
