alter table lots_transactions rename to lot_items;
alter table lot_items add column id serial primary key;
