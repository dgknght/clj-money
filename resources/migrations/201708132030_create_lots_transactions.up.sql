drop table lot_transactions;
create table lots_transactions (
  lot_id int not null,
  transaction_id int not null
);
create unique index uk_lots_transactions on lots_transactions (lot_id, transaction_id);
create index ix_lots_transactions_transaction_id on lots_transactions (transaction_id);
