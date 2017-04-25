alter table entities
  add column inventory_method char(4) default 'fifo',
  add column monitored_account_ids text;
alter table entities drop column settings;
