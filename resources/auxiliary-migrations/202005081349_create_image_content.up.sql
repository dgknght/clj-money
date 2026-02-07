-- Extensions and global settings
CREATE EXTENSION IF NOT EXISTS pgcrypto WITH SCHEMA public;
COMMENT ON EXTENSION pgcrypto IS 'cryptographic functions';
SET default_tablespace = '';
SET default_table_access_method = heap;

-- image_content table
CREATE SEQUENCE IF NOT EXISTS public.image_content_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.image_content_id_seq OWNER TO ddl_user;
CREATE TABLE IF NOT EXISTS public.image_content (
    id integer DEFAULT nextval('public.image_content_id_seq'::regclass) NOT NULL,
    uuid character(40) NOT NULL,
    content bytea NOT NULL
);
ALTER TABLE public.image_content OWNER TO ddl_user;
ALTER TABLE ONLY public.image_content
    ADD CONSTRAINT image_content_pkey PRIMARY KEY (id);
CREATE UNIQUE INDEX IF NOT EXISTS uk_image_content_uuid ON public.image_content USING btree (uuid);
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.image_content TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.image_content_id_seq TO app_user;
