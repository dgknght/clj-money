alter table imports
  add constraint fk_imports_user foreign key (user_id) references users (id) on delete cascade;
