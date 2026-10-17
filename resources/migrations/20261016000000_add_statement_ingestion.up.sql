CREATE SEQUENCE public.statement_ingestion_id_seq
    START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER SEQUENCE public.statement_ingestion_id_seq OWNER TO ddl_user;
CREATE TABLE public.statement_ingestion (
    id integer NOT NULL,
    entity_id integer NOT NULL,
    image_id integer NOT NULL,
    account_id integer,
    status character varying(20) NOT NULL DEFAULT 'pending',
    statement text,
    warnings text,
    error text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);
ALTER TABLE public.statement_ingestion OWNER TO ddl_user;
ALTER SEQUENCE public.statement_ingestion_id_seq
    OWNED BY public.statement_ingestion.id;
ALTER TABLE ONLY public.statement_ingestion
    ALTER COLUMN id SET DEFAULT
    nextval('public.statement_ingestion_id_seq'::regclass);
ALTER TABLE ONLY public.statement_ingestion
    ADD CONSTRAINT statement_ingestion_pkey PRIMARY KEY (id);
CREATE INDEX ix_statement_ingestion_entity_id
    ON public.statement_ingestion USING btree (entity_id);
CREATE INDEX ix_statement_ingestion_image_id
    ON public.statement_ingestion USING btree (image_id);
CREATE INDEX ix_statement_ingestion_account_id
    ON public.statement_ingestion USING btree (account_id);
ALTER TABLE ONLY public.statement_ingestion
    ADD CONSTRAINT fk_statement_ingestion_entity
    FOREIGN KEY (entity_id) REFERENCES public.entity(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.statement_ingestion
    ADD CONSTRAINT fk_statement_ingestion_image
    FOREIGN KEY (image_id) REFERENCES public.image(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.statement_ingestion
    ADD CONSTRAINT fk_statement_ingestion_account
    FOREIGN KEY (account_id) REFERENCES public.account(id) ON DELETE SET NULL;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.statement_ingestion TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.statement_ingestion_id_seq TO app_user;
