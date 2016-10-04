alter table accounts add column entity_id int not null;
drop index uk_accounts_name;
create unique index uk_accounts_name on accounts(entity_id, name);
