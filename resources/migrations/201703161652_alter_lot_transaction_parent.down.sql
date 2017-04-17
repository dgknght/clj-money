alter table lot_transactions
  add column account_id int not null,
  add column commodity_id int not null,
  drop column lot_id;

create index ix_lot_transactions_account_id on lot_transactions(account_id);
create index ix_lot_transactions_commodity_id on lot_transactions(commodity_id);
