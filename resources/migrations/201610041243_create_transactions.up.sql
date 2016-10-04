/*
 * transactions
 */
create sequence transactions_id_seq
  increment by 1
  start 1;

create table transactions (
  id int primary key not null default nextval('transactions_id_seq'),
  transaction_date date not null,
  entity_id int not null
);

create index ix_transactions_date on transactions (entity_id, transaction_date DESC);

alter sequence transactions_id_seq
  owned by transactions.id;

/*
 * transaction items
 */
create sequence transaction_items_id_seq
  increment by 1
  start 1;

create table transaction_items (
  id int primary key not null default nextval('transaction_items_id_seq'),
  transaction_id int not null,
  account_id int not null,
  previous_item_id int,
  next_item_id int,
  action varchar(10) not null,
  amount money not null,
  balance money not null
);

create index ix_transaction_items_account on transaction_items (account_id);
create index ix_transaction_items_transaction on transaction_items (transaction_id);
create index ix_transaction_items_previous on transaction_items (previous_item_id);
create index ix_transaction_items_next on transaction_items (next_item_id);

alter sequence transaction_items_id_seq
  owned by transaction_items.id;
