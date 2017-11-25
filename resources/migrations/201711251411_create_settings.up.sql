create table settings (
  name varchar(50) not null primary key,
  value text not null,
  created_at timestamp with time zone not null default now(),
  updated_at timestamp with time zone not null default now()
);
