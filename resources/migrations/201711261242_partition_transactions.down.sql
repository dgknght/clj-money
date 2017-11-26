alter table attachments drop column transaction_id;
alter table attachments
  add column transaction_id int not null,
  add constraint fk_attachments_transaction foreign key (transaction_id) references transactions (id) on delete cascade;

create index ix_attachments_transaction_id on attachments (transaction_id);

alter table lots_transactions drop column transaction_id;
alter table lots_transactions
  add column transaction_id int not null,
  add constraint fk_lots_transactions_transaction foreign key (transaction_id) references transactions (id) on delete cascade;

create unique index uk_lots_transactions_lot_id on lots_transactions (lot_id, transaction_id);
create index ix_lots_transactions_transaction_id on lots_transactions (transaction_id);

drop table transaction_items_base;
drop table transactions_base;
drop extension pgcrypto;
