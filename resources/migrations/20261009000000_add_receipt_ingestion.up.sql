CREATE SEQUENCE public.receipt_ingestion_id_seq
    START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER SEQUENCE public.receipt_ingestion_id_seq OWNER TO ddl_user;
CREATE TABLE public.receipt_ingestion (
    id integer NOT NULL,
    entity_id integer NOT NULL,
    image_id integer NOT NULL,
    status character varying(20) NOT NULL DEFAULT 'pending',
    receipt text,
    error text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);
ALTER TABLE public.receipt_ingestion OWNER TO ddl_user;
ALTER SEQUENCE public.receipt_ingestion_id_seq
    OWNED BY public.receipt_ingestion.id;
ALTER TABLE ONLY public.receipt_ingestion
    ALTER COLUMN id SET DEFAULT
    nextval('public.receipt_ingestion_id_seq'::regclass);
ALTER TABLE ONLY public.receipt_ingestion
    ADD CONSTRAINT receipt_ingestion_pkey PRIMARY KEY (id);
CREATE INDEX ix_receipt_ingestion_entity_id
    ON public.receipt_ingestion USING btree (entity_id);
CREATE INDEX ix_receipt_ingestion_image_id
    ON public.receipt_ingestion USING btree (image_id);
ALTER TABLE ONLY public.receipt_ingestion
    ADD CONSTRAINT fk_receipt_ingestion_entity
    FOREIGN KEY (entity_id) REFERENCES public.entity(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.receipt_ingestion
    ADD CONSTRAINT fk_receipt_ingestion_image
    FOREIGN KEY (image_id) REFERENCES public.image(id) ON DELETE CASCADE;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.receipt_ingestion TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.receipt_ingestion_id_seq TO app_user;
