CREATE SEQUENCE if not exists public.image_contents_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
create table if not exists image_contents (
    id integer DEFAULT nextval('public.image_contents_id_seq'::regclass) NOT NULL,
    uuid character(40) NOT NULL,
    content bytea NOT NULL
);
ALTER SEQUENCE public.image_contents_id_seq OWNED BY public.images.id;
ALTER TABLE ONLY public.image_contents
    ADD CONSTRAINT image_contents_pkey PRIMARY KEY (id);
CREATE UNIQUE INDEX if not exists uk_image_contents_uuid ON public.image_contents USING btree (uuid);

insert into image_contents (uuid, content)
select body_hash, body
from images;

alter table images drop column if exists body;
alter table images rename column body_hash to uuid;

GRANT SELECT, INSERT, UPDATE, DELETE ON public.image_contents TO app_user;
GRANT SELECT, INSERT, UPDATE, DELETE ON public.image_contents_id_seq TO app_user;
