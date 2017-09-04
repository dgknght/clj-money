alter table commodities
  add column "type" varchar(50),
  alter column exchange drop not null;

update commodities set "type" = 'stock';

alter table commodities
  alter column "type" set not null;
