create sequence identities_id_seq
  increment by 1
  start 1;

create table identities (
  id int primary key not null default nextval('identities_id_seq'),
  user_id int not null references users(id) on delete cascade,
  provider varchar(20) not null,
  provider_id varchar(255) not null
);

create unique index uk_identities_provider_id on identities (provider, provider_id);
create index ix_identities_user_id on identities (user_id);

alter sequence identities_id_seq
  owned by identities.id;
