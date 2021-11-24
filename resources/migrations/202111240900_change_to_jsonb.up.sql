alter table entities alter column settings type jsonb;
alter table budget_items alter column spec type jsonb;
alter table grants alter column permissions type jsonb;
alter table imports
  alter column progress type jsonb,
  alter column options type jsonb;
