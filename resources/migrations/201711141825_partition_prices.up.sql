create extension if not exists pgcrypto;

create table prices_base (
  id uuid not null primary key default gen_random_uuid(),
  trade_date date not null,
  commodity_id int not null,
  price numeric(10,2) not null,
  created_at timestamp with time zone not null default now(),
  updated_at timestamp with time zone not null default now()
);

alter table prices drop constraint fk_prices_commodity;

alter table prices_base
  add constraint fk_prices_commodity
      foreign key (commodity_id)
      references commodities (id)
      on delete cascade;
