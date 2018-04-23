alter table accounts rename quantity to balance;
alter table accounts drop column value;
alter table transaction_items_base rename quantity to amount;
