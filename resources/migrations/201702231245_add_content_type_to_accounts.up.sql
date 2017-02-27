alter table accounts
  add column content_type varchar(20);

update accounts set
  content_type = 'currency';

alter table accounts
 alter column content_type set not null;
