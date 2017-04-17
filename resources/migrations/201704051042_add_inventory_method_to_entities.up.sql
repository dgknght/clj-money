alter table entities
  add column inventory_method char(4) not null default('fifo');
