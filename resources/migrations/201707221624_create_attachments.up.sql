create sequence attachments_id_seq
  increment by 1
  start 1;

create table attachments (
  id int primary key not null default nextval('attachments_id_seq'),
  transaction_id int not null,
  caption varchar(255) not null,
  image_id int not null,
  mime_type varchar(100) not null,
  created_at timestamp with time zone not null default now(),
  updated_at timestamp with time zone not null default now()
);

create index ix_attachments_transaction_id on attachments (transaction_id);
create index ix_attachments_image_id on attachments (image_id);

alter sequence attachments_id_seq
  owned by attachments.id;
