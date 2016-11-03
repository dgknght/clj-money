alter table accounts add column balance decimal(10, 2);
update accounts set balance = 0;
alter table accounts alter column balance set not null;
