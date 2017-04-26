alter table lot_transactions add column transaction_id int;
create index ix_lot_transactions_transaction_id on lot_transactions(transaction_id);
