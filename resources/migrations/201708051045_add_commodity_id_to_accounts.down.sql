alter table accounts
  add column content_type varchar(20) not null,
  drop column commodity_id;
