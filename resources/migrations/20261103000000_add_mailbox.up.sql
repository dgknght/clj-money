CREATE SEQUENCE public.mailbox_id_seq
    START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER SEQUENCE public.mailbox_id_seq OWNER TO ddl_user;
CREATE TABLE public.mailbox (
    id integer NOT NULL,
    entity_id integer NOT NULL,
    host character varying(255) NOT NULL,
    port integer NOT NULL,
    username character varying(255) NOT NULL,
    password text NOT NULL,
    folder character varying(255) NOT NULL DEFAULT 'INBOX',
    allowed_senders text,
    uid_validity bigint,
    last_uid bigint,
    checked_at timestamp with time zone,
    error text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);
ALTER TABLE public.mailbox OWNER TO ddl_user;
ALTER SEQUENCE public.mailbox_id_seq
    OWNED BY public.mailbox.id;
ALTER TABLE ONLY public.mailbox
    ALTER COLUMN id SET DEFAULT
    nextval('public.mailbox_id_seq'::regclass);
ALTER TABLE ONLY public.mailbox
    ADD CONSTRAINT mailbox_pkey PRIMARY KEY (id);
CREATE UNIQUE INDEX uk_mailbox_entity_id
    ON public.mailbox USING btree (entity_id);
ALTER TABLE ONLY public.mailbox
    ADD CONSTRAINT fk_mailbox_entity
    FOREIGN KEY (entity_id) REFERENCES public.entity(id) ON DELETE CASCADE;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.mailbox TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.mailbox_id_seq TO app_user;
