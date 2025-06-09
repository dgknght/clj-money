alter table lots_transactions rename to lot_items;
alter table lot_items add column id serial primary key;
GRANT SELECT, INSERT, UPDATE, DELETE ON public.lot_items_id_seq TO app_user;
