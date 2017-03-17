alter table lot_transactions
  add column lot_id int not null,
  drop column account_id,
  drop column commodity_id;

create index ix_lot_transaction_lot_id on lot_transactions (lot_id);
