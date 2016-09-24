drop index uk_accounts_name;
alter table accounts drop column entity_id;
create unique index uk_accounts_name on accounts(name);
