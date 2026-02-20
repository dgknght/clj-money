CREATE SEQUENCE public.memo_ledger_entry_id_seq
    START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER SEQUENCE public.memo_ledger_entry_id_seq OWNER TO ddl_user;
CREATE TABLE public.memo_ledger_entry (
    id integer NOT NULL,
    lot_id integer NOT NULL,
    transaction_date date NOT NULL,
    memo character varying(500) NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);
ALTER TABLE public.memo_ledger_entry OWNER TO ddl_user;
ALTER SEQUENCE public.memo_ledger_entry_id_seq
    OWNED BY public.memo_ledger_entry.id;
ALTER TABLE ONLY public.memo_ledger_entry
    ALTER COLUMN id SET DEFAULT
    nextval('public.memo_ledger_entry_id_seq'::regclass);
ALTER TABLE ONLY public.memo_ledger_entry
    ADD CONSTRAINT memo_ledger_entry_pkey PRIMARY KEY (id);
CREATE INDEX ix_memo_ledger_entry_lot_id
    ON public.memo_ledger_entry USING btree (lot_id);
ALTER TABLE ONLY public.memo_ledger_entry
    ADD CONSTRAINT fk_memo_ledger_entry_lot
    FOREIGN KEY (lot_id) REFERENCES public.lot(id) ON DELETE CASCADE;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.memo_ledger_entry TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.memo_ledger_entry_id_seq TO app_user;
