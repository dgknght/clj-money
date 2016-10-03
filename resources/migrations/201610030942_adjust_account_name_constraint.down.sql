drop index uk_accounts_name;
create unique index uk_accounts_name on accounts(entity_id, name);
