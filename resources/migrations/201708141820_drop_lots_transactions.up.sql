drop table lots_transactions;
alter table transactions add column lot_id integer;
create unique index uk_transactions_lot_id on transactions(lot_id);
