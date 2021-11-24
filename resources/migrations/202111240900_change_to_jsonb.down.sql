alter table entities alter column settings type json;
alter table budget_items alter column spec type json;
alter table grants alter column permissions type json;
alter table imports
  alter column progress type json,
  alter column options type json;
