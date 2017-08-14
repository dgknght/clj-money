drop table lots_transactions;

create sequence lot_transactions_id_seq
  increment by 1
  start 1;

create table lot_transactions (
  id int primary key not null default nextval('lot_transactions_id_seq'),
  account_id int not null,
  commodity_id int not null,
  trade_date bigint not null,
  action varchar(10) not null,
  shares decimal(10, 4) not null,
  price decimal(10, 4) not null,
  created_at timestamp with time zone not null default now(),
  updated_at timestamp with time zone not null default now()
);

create index ix_lot_transactions_account_id on lot_transactions(account_id);
create index ix_lot_transactions_commodity_id on lot_transactions(commodity_id);

alter sequence lot_transactions_id_seq
  owned by lot_transactions.id;
