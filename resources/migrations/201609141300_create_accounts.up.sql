create sequence accounts_id_seq
  increment by 1
  start 1;

create table accounts (
  id int primary key not null default nextval('accounts_id_seq'),
  name varchar(50) not null
);

alter sequence accounts_id_seq
  owned by accounts.id;
