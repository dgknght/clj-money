create sequence images_id_seq
  increment by 1
  start 1;

create table images (
  id int primary key not null default nextval('imports_id_seq'),
  user_id int not null,
  original_filename varchar(255) not null,
  body_hash char(40),
  body bytea not null,
  created_at timestamp with time zone not null default now()
);

alter sequence images_id_seq
  owned by images.id;

create unique index uk_images_user_hash on images (user_id, body_hash);

alter table imports
  drop column content,
  add column image_id int not null;

create index ix_imports_image_id on imports (image_id);
