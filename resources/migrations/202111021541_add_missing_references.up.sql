alter table lots_transactions add constraint fk_transactions_lots_transactions foreign key (transaction_date, transaction_id) references transactions(transaction_date, id) on delete cascade;
