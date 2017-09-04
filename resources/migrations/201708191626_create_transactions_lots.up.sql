alter table transactions drop column lot_items;

create table lots_transactions (
  lot_id int not null,
  transaction_id int not null,
  price numeric(12,4) not null,
  shares numeric(10,4) not null,
  lot_action varchar(10) not null
);

create unique index uk_lots_transactions_lot_id on lots_transactions(lot_id, transaction_id);
create unique index uk_lots_transactions_transaction_id on lots_transactions(transaction_id);
