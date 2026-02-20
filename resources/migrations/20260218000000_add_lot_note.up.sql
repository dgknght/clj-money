CREATE SEQUENCE public.lot_note_id_seq
    START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER SEQUENCE public.lot_note_id_seq OWNER TO ddl_user;
CREATE TABLE public.lot_note (
    id integer NOT NULL,
    lot_id integer NOT NULL,
    transaction_date date NOT NULL,
    memo character varying(500) NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);
ALTER TABLE public.lot_note OWNER TO ddl_user;
ALTER SEQUENCE public.lot_note_id_seq
    OWNED BY public.lot_note.id;
ALTER TABLE ONLY public.lot_note
    ALTER COLUMN id SET DEFAULT
    nextval('public.lot_note_id_seq'::regclass);
ALTER TABLE ONLY public.lot_note
    ADD CONSTRAINT lot_note_pkey PRIMARY KEY (id);
CREATE INDEX ix_lot_note_lot_id
    ON public.lot_note USING btree (lot_id);
ALTER TABLE ONLY public.lot_note
    ADD CONSTRAINT fk_lot_note_lot
    FOREIGN KEY (lot_id) REFERENCES public.lot(id) ON DELETE CASCADE;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.lot_note TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.lot_note_id_seq TO app_user;
