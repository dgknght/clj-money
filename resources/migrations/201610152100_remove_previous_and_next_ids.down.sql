alter table transaction_items
  add column previous_item_id int,
  add column next_item_id int;
