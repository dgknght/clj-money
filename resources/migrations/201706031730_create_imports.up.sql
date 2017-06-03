create sequence imports_id_seq
  increment by 1
  start 1;

create table imports (
  id int primary key not null default nextval('imports_id_seq'),
  user_id int not null,
  content bytea not null,
  filename varchar(255) not null,
  record_counts jsonb,
  created_at timestamp with time zone not null default now(),
  updated_at timestamp with time zone not null default now()
);

create index ix_imports_user_id on imports (user_id);

alter sequence imports_id_seq
  owned by imports.id;
