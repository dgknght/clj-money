create sequence reconciliations_id_seq
  increment by 1
  start 1;

create table reconciliations (
  id int primary key not null default nextval('reconciliations_id_seq'),
  account_id int not null,
  end_of_period bigint not null,
  status varchar(20) not null
);

create index ix_reconciliations_account on reconciliations (account_id, end_of_period);

alter sequence reconciliations_id_seq
  owned by reconciliations.id;

alter table transaction_items
  add column reconciliation_id int;

create index ix_transaction_items_reconciliation on transaction_items (reconciliation_id);
