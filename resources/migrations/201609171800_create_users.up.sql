create sequence users_id_seq
  increment by 1
  start 1;

create table users (
  id int primary key not null default nextval('users_id_seq'),
  email varchar(100) not null,
  password varchar(100) not null,
  first_name varchar(100) not null,
  last_name varchar(100) not null
);

create unique index uk_users_email on users(email);

alter sequence users_id_seq
  owned by users.id;
