create sequence entities_id_seq
  increment by 1
  start 1;

create table entities (
  id int primary key not null default nextval('entities_id_seq'),
  user_id int not null,
  name varchar(100) not null
);

create unique index uk_entities_name on entities(user_id, name);

alter sequence entities_id_seq
  owned by entities.id;
