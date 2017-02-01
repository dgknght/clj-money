alter table reconciliations add column balance decimal(10,2);
update reconciliations set balance = 0;
alter table reconciliations alter column balance set not null;
