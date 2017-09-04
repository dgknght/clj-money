alter table commodities
  drop column "type",
  alter column exchange set not null;
