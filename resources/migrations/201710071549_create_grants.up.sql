create sequence grants_id_seq
  increment by 1
  start 1;

create table grants (
  id int primary key not null default nextval('grants_id_seq'),
  entity_id int not null,
  user_id int not null,
  permissions text,
  created_at timestamp with time zone not null default now(),
  updated_at timestamp with time zone not null default now()
);
alter table grants
  add constraint fk_grants_entities foreign key (entity_id) references entities (id) on delete cascade,
  add constraint fk_grants_users foreign key (user_id) references users (id) on delete cascade;

create unique index uk_grants_entity_user on grants (entity_id, user_id);
create index ix_grants_user on grants(user_id);

alter sequence grants_id_seq
  owned by grants.id;
