alter table entities add column settings text;
update entities set settings = '{:inventory_method "' || inventory_method || '"}'
  where inventory_method is not null;
alter table entities
  drop column inventory_method,
  drop column monitored_account_ids;
