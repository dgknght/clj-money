create table cli_auth_sessions (
  device_code varchar(36) not null primary key,
  user_code varchar(8) not null unique,
  user_id integer null references users(id) on delete cascade,
  status varchar(20) not null default 'pending',
  expires_at timestamp not null,
  created_at timestamp not null default now(),
  approved_at timestamp null,
  constraint valid_status check (status in ('pending', 'approved', 'denied'))
);

create index idx_cli_auth_sessions_user_code on cli_auth_sessions(user_code);
create index idx_cli_auth_sessions_expires_at on cli_auth_sessions(expires_at);
create index idx_cli_auth_sessions_user_id on cli_auth_sessions(user_id);
