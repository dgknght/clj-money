alter table transaction_items
add constraint transaction_items_transaction_id_fkey foreign key (transaction_date, transaction_id) references transactions (transaction_date, id) on delete cascade;
