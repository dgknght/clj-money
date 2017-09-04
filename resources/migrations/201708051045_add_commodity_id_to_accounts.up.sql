alter table accounts
  drop column content_type,
  add column commodity_id int not null;
