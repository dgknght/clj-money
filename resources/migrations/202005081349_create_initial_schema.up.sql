-- Extensions and global settings
CREATE EXTENSION IF NOT EXISTS pgcrypto WITH SCHEMA public;
COMMENT ON EXTENSION pgcrypto IS 'cryptographic functions';
SET default_tablespace = '';
SET default_table_access_method = heap;

-- user table
CREATE SEQUENCE public.user_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.user_id_seq OWNER TO ddl_user;
CREATE TABLE public."user" (
    id integer NOT NULL,
    email character varying(100) NOT NULL,
    password character varying(100) NOT NULL,
    first_name character varying(100) NOT NULL,
    last_name character varying(100) NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    password_reset_token character(32),
    token_expires_at timestamp without time zone
);
ALTER TABLE public."user" OWNER TO ddl_user;
ALTER SEQUENCE public.user_id_seq OWNED BY public."user".id;
ALTER TABLE ONLY public."user" ALTER COLUMN id SET DEFAULT nextval('public.user_id_seq'::regclass);
ALTER TABLE ONLY public."user"
    ADD CONSTRAINT user_pkey PRIMARY KEY (id);
CREATE UNIQUE INDEX uk_user_email ON public."user" USING btree (email);
CREATE INDEX uk_user_password_reset_token ON public."user" USING btree (password_reset_token);
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public."user" TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.user_id_seq TO app_user;

-- image_content table
CREATE SEQUENCE public.image_content_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.image_content_id_seq OWNER TO ddl_user;
CREATE TABLE public.image_content (
    id integer DEFAULT nextval('public.image_content_id_seq'::regclass) NOT NULL,
    uuid character(40) NOT NULL,
    content bytea NOT NULL
);
ALTER TABLE public.image_content OWNER TO ddl_user;
ALTER TABLE ONLY public.image_content
    ADD CONSTRAINT image_content_pkey PRIMARY KEY (id);
CREATE UNIQUE INDEX uk_image_content_uuid ON public.image_content USING btree (uuid);
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.image_content TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.image_content_id_seq TO app_user;

-- entity table
CREATE SEQUENCE public.entity_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.entity_id_seq OWNER TO ddl_user;
CREATE TABLE public.entity (
    id integer NOT NULL,
    user_id integer NOT NULL,
    name character varying(100) NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    settings jsonb,
    price_date_range date[],
    transaction_date_range date[]
);
ALTER TABLE public.entity OWNER TO ddl_user;
ALTER SEQUENCE public.entity_id_seq OWNED BY public.entity.id;
ALTER TABLE ONLY public.entity ALTER COLUMN id SET DEFAULT nextval('public.entity_id_seq'::regclass);
ALTER TABLE ONLY public.entity
    ADD CONSTRAINT entity_pkey PRIMARY KEY (id);
CREATE UNIQUE INDEX uk_entity_name ON public.entity USING btree (user_id, name);
ALTER TABLE ONLY public.entity
    ADD CONSTRAINT fk_entity_user FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.entity TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.entity_id_seq TO app_user;

-- identity table
CREATE SEQUENCE public.identity_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.identity_id_seq OWNER TO ddl_user;
CREATE TABLE public.identity (
    id integer NOT NULL,
    user_id integer NOT NULL,
    provider character varying(20) NOT NULL,
    provider_id character varying(255) NOT NULL
);
ALTER TABLE public.identity OWNER TO ddl_user;
ALTER SEQUENCE public.identity_id_seq OWNED BY public.identity.id;
ALTER TABLE ONLY public.identity ALTER COLUMN id SET DEFAULT nextval('public.identity_id_seq'::regclass);
ALTER TABLE ONLY public.identity
    ADD CONSTRAINT identity_pkey PRIMARY KEY (id);
CREATE INDEX ix_identity_user_id ON public.identity USING btree (user_id);
CREATE UNIQUE INDEX uk_identity_provider_id ON public.identity USING btree (provider, provider_id);
ALTER TABLE ONLY public.identity
    ADD CONSTRAINT identity_user_id_fkey FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.identity TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.identity_id_seq TO app_user;

-- image table
CREATE SEQUENCE public.image_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.image_id_seq OWNER TO ddl_user;
CREATE TABLE public.image (
    id integer NOT NULL,
    user_id integer NOT NULL,
    original_filename character varying(255) NOT NULL,
    uuid character(40) NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    content_type character varying(100) NOT NULL
);
ALTER TABLE public.image OWNER TO ddl_user;
ALTER SEQUENCE public.image_id_seq OWNED BY public.image.id;
ALTER TABLE ONLY public.image ALTER COLUMN id SET DEFAULT nextval('public.image_id_seq'::regclass);
ALTER TABLE ONLY public.image
    ADD CONSTRAINT image_pkey PRIMARY KEY (id);
CREATE UNIQUE INDEX uk_image_user_hash ON public.image USING btree (user_id, uuid);
ALTER TABLE ONLY public.image
    ADD CONSTRAINT fk_image_user FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.image TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.image_id_seq TO app_user;

-- import table
CREATE SEQUENCE public.import_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.import_id_seq OWNER TO ddl_user;
CREATE TABLE public.import (
    id integer NOT NULL,
    user_id integer NOT NULL,
    entity_name character varying(100) NOT NULL,
    progress jsonb,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    image_ids integer[] NOT NULL,
    options jsonb
);
ALTER TABLE public.import OWNER TO ddl_user;
ALTER SEQUENCE public.import_id_seq OWNED BY public.import.id;
ALTER TABLE ONLY public.import ALTER COLUMN id SET DEFAULT nextval('public.import_id_seq'::regclass);
ALTER TABLE ONLY public.import
    ADD CONSTRAINT import_pkey PRIMARY KEY (id);
CREATE INDEX ix_import_user_id ON public.import USING btree (user_id);
ALTER TABLE ONLY public.import
    ADD CONSTRAINT fk_import_user FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.import TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.import_id_seq TO app_user;

-- grant table
CREATE SEQUENCE public.grant_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.grant_id_seq OWNER TO ddl_user;
CREATE TABLE public."grant" (
    id integer NOT NULL,
    entity_id integer NOT NULL,
    user_id integer NOT NULL,
    permissions jsonb,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);
ALTER TABLE public."grant" OWNER TO ddl_user;
ALTER SEQUENCE public.grant_id_seq OWNED BY public."grant".id;
ALTER TABLE ONLY public."grant" ALTER COLUMN id SET DEFAULT nextval('public.grant_id_seq'::regclass);
ALTER TABLE ONLY public."grant"
    ADD CONSTRAINT grant_pkey PRIMARY KEY (id);
CREATE INDEX ix_grant_user ON public."grant" USING btree (user_id);
CREATE UNIQUE INDEX uk_grant_entity_user ON public."grant" USING btree (entity_id, user_id);
ALTER TABLE ONLY public."grant"
    ADD CONSTRAINT fk_grant_entity FOREIGN KEY (entity_id) REFERENCES public.entity(id) ON DELETE CASCADE;
ALTER TABLE ONLY public."grant"
    ADD CONSTRAINT fk_grant_user FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public."grant" TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.grant_id_seq TO app_user;

-- commodity table
CREATE SEQUENCE public.commodity_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.commodity_id_seq OWNER TO ddl_user;
CREATE TABLE public.commodity (
    id integer NOT NULL,
    entity_id integer NOT NULL,
    name character varying(100) NOT NULL,
    symbol character varying(10) NOT NULL,
    exchange character varying(10),
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    type character varying(50) NOT NULL,
    price_config jsonb NOT NULL,
    price_date_range date[]
);
ALTER TABLE public.commodity OWNER TO ddl_user;
ALTER SEQUENCE public.commodity_id_seq OWNED BY public.commodity.id;
ALTER TABLE ONLY public.commodity ALTER COLUMN id SET DEFAULT nextval('public.commodity_id_seq'::regclass);
ALTER TABLE ONLY public.commodity
    ADD CONSTRAINT commodity_pkey PRIMARY KEY (id);
CREATE UNIQUE INDEX uk_commodity_name ON public.commodity USING btree (entity_id, exchange, name);
CREATE UNIQUE INDEX uk_commodity_symbol ON public.commodity USING btree (entity_id, exchange, symbol);
ALTER TABLE ONLY public.commodity
    ADD CONSTRAINT fk_commodity_entity FOREIGN KEY (entity_id) REFERENCES public.entity(id) ON DELETE CASCADE;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.commodity TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.commodity_id_seq TO app_user;

-- cached_price table
CREATE TABLE public.cached_price (
    trade_date date NOT NULL,
    id uuid DEFAULT public.gen_random_uuid() NOT NULL,
    symbol character varying(10) NOT NULL,
    exchange character varying(10) NOT NULL,
    value numeric(19,6) NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
)
PARTITION BY RANGE (trade_date);
ALTER TABLE public.cached_price OWNER TO ddl_user;
ALTER TABLE ONLY public.cached_price
    ADD CONSTRAINT pk_cached_price PRIMARY KEY (trade_date, id);
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.cached_price TO app_user;

-- account table
CREATE SEQUENCE public.account_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.account_id_seq OWNER TO ddl_user;
CREATE TABLE public.account (
    id integer NOT NULL,
    name character varying(255) NOT NULL,
    type character varying(20) NOT NULL,
    entity_id integer NOT NULL,
    parent_id integer,
    quantity numeric(19,6) NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    commodity_id integer NOT NULL,
    system_tags text[],
    value numeric(19,6),
    hidden boolean DEFAULT false,
    user_tags text[],
    price_as_of date,
    allocations jsonb,
    transaction_date_range date[]
);
ALTER TABLE public.account OWNER TO ddl_user;
ALTER SEQUENCE public.account_id_seq OWNED BY public.account.id;
ALTER TABLE ONLY public.account ALTER COLUMN id SET DEFAULT nextval('public.account_id_seq'::regclass);
ALTER TABLE ONLY public.account
    ADD CONSTRAINT account_pkey PRIMARY KEY (id);
CREATE UNIQUE INDEX uk_account_name ON public.account USING btree (entity_id, parent_id, name);
ALTER TABLE ONLY public.account
    ADD CONSTRAINT fk_account_account FOREIGN KEY (parent_id) REFERENCES public.account(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.account
    ADD CONSTRAINT fk_account_entity FOREIGN KEY (entity_id) REFERENCES public.entity(id) ON DELETE CASCADE;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.account TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.account_id_seq TO app_user;

-- price table
CREATE TABLE public.price (
    trade_date date NOT NULL,
    id uuid DEFAULT public.gen_random_uuid() NOT NULL,
    commodity_id integer NOT NULL,
    value numeric(19,6) NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
)
PARTITION BY RANGE (trade_date);
ALTER TABLE public.price OWNER TO ddl_user;
ALTER TABLE ONLY public.price
    ADD CONSTRAINT price_pkey PRIMARY KEY (trade_date, id);
CREATE INDEX ix_price_trade_date_commodity_id ON ONLY public.price USING btree (trade_date, commodity_id);
CREATE INDEX ix_price_trade_date_id ON ONLY public.price USING btree (trade_date, id);
ALTER TABLE public.price
    ADD CONSTRAINT fk_price_commodity FOREIGN KEY (commodity_id) REFERENCES public.commodity(id) ON DELETE CASCADE;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.price TO app_user;

-- budget table
CREATE SEQUENCE public.budget_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.budget_id_seq OWNER TO ddl_user;
CREATE TABLE public.budget (
    id integer NOT NULL,
    entity_id integer NOT NULL,
    name character varying(50) NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    start_date date NOT NULL,
    end_date date NOT NULL,
    period text[] NOT NULL
);
ALTER TABLE public.budget OWNER TO ddl_user;
ALTER SEQUENCE public.budget_id_seq OWNED BY public.budget.id;
ALTER TABLE ONLY public.budget ALTER COLUMN id SET DEFAULT nextval('public.budget_id_seq'::regclass);
ALTER TABLE ONLY public.budget
    ADD CONSTRAINT budget_pkey PRIMARY KEY (id);
CREATE UNIQUE INDEX uk_budget_name ON public.budget USING btree (entity_id, name);
ALTER TABLE ONLY public.budget
    ADD CONSTRAINT fk_budget_entity FOREIGN KEY (entity_id) REFERENCES public.entity(id) ON DELETE CASCADE;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.budget TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.budget_id_seq TO app_user;

-- budget_item table
CREATE SEQUENCE public.budget_item_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.budget_item_id_seq OWNER TO ddl_user;
CREATE TABLE public.budget_item (
    id integer NOT NULL,
    budget_id integer NOT NULL,
    account_id integer NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    periods numeric(19,6)[] NOT NULL,
    spec jsonb
);
ALTER TABLE public.budget_item OWNER TO ddl_user;
ALTER SEQUENCE public.budget_item_id_seq OWNED BY public.budget_item.id;
ALTER TABLE ONLY public.budget_item ALTER COLUMN id SET DEFAULT nextval('public.budget_item_id_seq'::regclass);
ALTER TABLE ONLY public.budget_item
    ADD CONSTRAINT budget_item_pkey PRIMARY KEY (id);
CREATE UNIQUE INDEX uk_budget_item_account ON public.budget_item USING btree (budget_id, account_id);
ALTER TABLE ONLY public.budget_item
    ADD CONSTRAINT fk_budget_item_account FOREIGN KEY (account_id) REFERENCES public.account(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.budget_item
    ADD CONSTRAINT fk_budget_item_budget FOREIGN KEY (budget_id) REFERENCES public.budget(id) ON DELETE CASCADE;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.budget_item TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.budget_item_id_seq TO app_user;

-- reconciliation table
CREATE SEQUENCE public.reconciliation_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.reconciliation_id_seq OWNER TO ddl_user;
CREATE TABLE public.reconciliation (
    id integer NOT NULL,
    end_of_period date NOT NULL,
    account_id integer NOT NULL,
    status character varying(20) NOT NULL,
    balance numeric(19,6) NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);
ALTER TABLE public.reconciliation OWNER TO ddl_user;
ALTER SEQUENCE public.reconciliation_id_seq OWNED BY public.reconciliation.id;
ALTER TABLE ONLY public.reconciliation ALTER COLUMN id SET DEFAULT nextval('public.reconciliation_id_seq'::regclass);
ALTER TABLE ONLY public.reconciliation
    ADD CONSTRAINT reconciliation_pkey PRIMARY KEY (id);
CREATE INDEX ix_reconciliation_eop_account_id ON public.reconciliation USING btree (end_of_period, account_id);
CREATE INDEX ix_reconciliation_account_id ON public.reconciliation USING btree (account_id);
ALTER TABLE public.reconciliation
    ADD CONSTRAINT fk_reconciliation_account FOREIGN KEY (account_id) REFERENCES public.account(id) ON DELETE CASCADE;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.reconciliation TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.reconciliation_id_seq TO app_user;

-- lot table
CREATE SEQUENCE public.lot_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.lot_id_seq OWNER TO ddl_user;
CREATE TABLE public.lot (
    id integer NOT NULL,
    commodity_id integer NOT NULL,
    account_id integer NOT NULL,
    shares_purchased numeric(19,6) NOT NULL,
    shares_owned numeric(19,6) DEFAULT 0 NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    purchase_price numeric(19,6) NOT NULL,
    purchase_date date NOT NULL
);
ALTER TABLE public.lot OWNER TO ddl_user;
ALTER SEQUENCE public.lot_id_seq OWNED BY public.lot.id;
ALTER TABLE ONLY public.lot ALTER COLUMN id SET DEFAULT nextval('public.lot_id_seq'::regclass);
ALTER TABLE ONLY public.lot
    ADD CONSTRAINT lot_pkey PRIMARY KEY (id);
CREATE INDEX ix_lot_account_id ON public.lot USING btree (account_id);
CREATE INDEX ix_lot_commodity_id ON public.lot USING btree (commodity_id);
ALTER TABLE ONLY public.lot
    ADD CONSTRAINT fk_lot_account FOREIGN KEY (account_id) REFERENCES public.account(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.lot
    ADD CONSTRAINT fk_lot_commodity FOREIGN KEY (commodity_id) REFERENCES public.commodity(id) ON DELETE CASCADE;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.lot TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.lot_id_seq TO app_user;

-- scheduled_transaction table
CREATE SEQUENCE public.scheduled_transaction_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.scheduled_transaction_id_seq OWNER TO ddl_user;
CREATE TABLE public.scheduled_transaction (
    id integer NOT NULL,
    start_date date NOT NULL,
    end_date date,
    enabled boolean DEFAULT true NOT NULL,
    last_occurrence date,
    date_spec jsonb NOT NULL,
    entity_id integer NOT NULL,
    description character varying(200) NOT NULL,
    memo character varying(200),
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    period text[] NOT NULL
);
ALTER TABLE public.scheduled_transaction OWNER TO ddl_user;
ALTER SEQUENCE public.scheduled_transaction_id_seq OWNED BY public.scheduled_transaction.id;
ALTER TABLE ONLY public.scheduled_transaction ALTER COLUMN id SET DEFAULT nextval('public.scheduled_transaction_id_seq'::regclass);
ALTER TABLE ONLY public.scheduled_transaction
    ADD CONSTRAINT scheduled_transaction_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.scheduled_transaction
    ADD CONSTRAINT fk_scheduled_transaction_entity FOREIGN KEY (entity_id) REFERENCES public.entity(id) ON DELETE CASCADE;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.scheduled_transaction TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.scheduled_transaction_id_seq TO app_user;

-- scheduled_transaction_item table
CREATE SEQUENCE public.scheduled_transaction_item_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.scheduled_transaction_item_id_seq OWNER TO ddl_user;
CREATE TABLE public.scheduled_transaction_item (
    id integer NOT NULL,
    scheduled_transaction_id integer NOT NULL,
    account_id integer NOT NULL,
    action character varying(10) NOT NULL,
    quantity numeric(19,6),
    memo character varying(200),
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);
ALTER TABLE public.scheduled_transaction_item OWNER TO ddl_user;
ALTER SEQUENCE public.scheduled_transaction_item_id_seq OWNED BY public.scheduled_transaction_item.id;
ALTER TABLE ONLY public.scheduled_transaction_item ALTER COLUMN id SET DEFAULT nextval('public.scheduled_transaction_item_id_seq'::regclass);
ALTER TABLE ONLY public.scheduled_transaction_item
    ADD CONSTRAINT scheduled_transaction_item_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.scheduled_transaction_item
    ADD CONSTRAINT fk_scheduled_transaction_item_account FOREIGN KEY (account_id) REFERENCES public.account(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.scheduled_transaction_item
    ADD CONSTRAINT fk_scheduled_transaction_item_scheduled_transaction FOREIGN KEY (scheduled_transaction_id) REFERENCES public.scheduled_transaction(id) ON DELETE CASCADE;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.scheduled_transaction_item TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.scheduled_transaction_item_id_seq TO app_user;

-- transaction table
CREATE SEQUENCE public.transaction_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.transaction_id_seq OWNER TO ddl_user;
CREATE TABLE public.transaction (
    id integer NOT NULL,
    transaction_date date NOT NULL,
    entity_id integer NOT NULL,
    description character varying(200) NOT NULL,
    memo character varying(200),
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    value numeric(19,6) DEFAULT 0 NOT NULL,
    scheduled_transaction_id integer,
    attachment_count smallint DEFAULT 0 NOT NULL
);
ALTER TABLE public.transaction OWNER TO ddl_user;
ALTER SEQUENCE public.transaction_id_seq OWNED BY public.transaction.id;
ALTER TABLE ONLY public.transaction ALTER COLUMN id SET DEFAULT nextval('public.transaction_id_seq'::regclass);
ALTER TABLE ONLY public.transaction
    ADD CONSTRAINT transaction_pkey PRIMARY KEY (id);
CREATE INDEX ix_transaction_scheduled_transaction_id ON public.transaction USING btree (scheduled_transaction_id);
CREATE INDEX ix_transaction_transaction_date_entity_id ON public.transaction USING btree (transaction_date, entity_id);
CREATE INDEX ix_transaction_entity_id ON public.transaction USING btree (entity_id);
ALTER TABLE public.transaction
    ADD CONSTRAINT transaction_entity_id_fkey FOREIGN KEY (entity_id) REFERENCES public.entity(id) ON DELETE CASCADE;
ALTER TABLE public.transaction
    ADD CONSTRAINT transaction_scheduled_transaction_id_fkey FOREIGN KEY (scheduled_transaction_id) REFERENCES public.scheduled_transaction(id);
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.transaction TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.transaction_id_seq TO app_user;

-- attachment table
CREATE SEQUENCE public.attachment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.attachment_id_seq OWNER TO ddl_user;
CREATE TABLE public.attachment (
    id integer NOT NULL,
    caption character varying(255),
    image_id integer NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    transaction_id integer NOT NULL
);
ALTER TABLE public.attachment OWNER TO ddl_user;
ALTER SEQUENCE public.attachment_id_seq OWNED BY public.attachment.id;
ALTER TABLE ONLY public.attachment ALTER COLUMN id SET DEFAULT nextval('public.attachment_id_seq'::regclass);
ALTER TABLE ONLY public.attachment
    ADD CONSTRAINT attachment_pkey PRIMARY KEY (id);
CREATE INDEX ix_attachment_image_id ON public.attachment USING btree (image_id);
ALTER TABLE ONLY public.attachment
    ADD CONSTRAINT fk_attachment_image FOREIGN KEY (image_id) REFERENCES public.image(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.attachment
    ADD CONSTRAINT fk_attachment_transaction FOREIGN KEY (transaction_id) REFERENCES public.transaction(id) ON DELETE CASCADE;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.attachment TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.attachment_id_seq TO app_user;

-- transaction_item table
CREATE TABLE public.transaction_item (
    id uuid DEFAULT public.gen_random_uuid() NOT NULL,
    transaction_id integer NOT NULL,
    credit_item_id uuid NOT NULL,
    debit_item_id uuid NOT NULL,
    value numeric(19,6),
    memo character varying(200),
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);
ALTER TABLE public.transaction_item OWNER TO ddl_user;
ALTER TABLE ONLY public.transaction_item
    ADD CONSTRAINT transaction_item_pkey PRIMARY KEY (id);
CREATE INDEX ix_transaction_item_credit_item_id ON public.transaction_item USING btree (credit_item_id);
CREATE INDEX ix_transaction_item_debit_item_id ON public.transaction_item USING btree (debit_item_id);
CREATE INDEX ix_transaction_item_transaction_id ON public.transaction_item USING btree (transaction_id);
ALTER TABLE public.transaction_item
    ADD CONSTRAINT transaction_item_transaction_id_fkey FOREIGN KEY (transaction_id) REFERENCES public.transaction(id) ON DELETE CASCADE;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.transaction_item TO app_user;

-- account_item table
CREATE TABLE public.account_item (
    id uuid DEFAULT public.gen_random_uuid() NOT NULL,
    transaction_item_id uuid,
    account_id integer NOT NULL,
    action varchar(7),
    reconciliation_id integer,
    quantity numeric(19,6),
    balance numeric(19,6),
    index integer,
    memo character varying(200),
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);
ALTER TABLE public.account_item OWNER TO ddl_user;
ALTER TABLE ONLY public.account_item
    ADD CONSTRAINT account_item_pkey PRIMARY KEY (id);
CREATE INDEX ix_account_item_transaction_item_id ON public.account_item USING btree (transaction_item_id);
CREATE INDEX ix_account_item_account_id ON public.account_item USING btree (account_id);
CREATE INDEX ix_account_item_reconciliation_id ON public.account_item USING btree (reconciliation_id);
ALTER TABLE public.account_item
    ADD CONSTRAINT account_item_reconciliation_id_fkey FOREIGN KEY (reconciliation_id) REFERENCES public.reconciliation(id) ON DELETE SET NULL;
ALTER TABLE public.account_item
    ADD CONSTRAINT account_item_account_id_fkey FOREIGN KEY (account_id) REFERENCES public.account(id) ON DELETE CASCADE;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.account_item TO app_user;

-- transaction_item foreign keys to account_item (must come after account_item table)
ALTER TABLE public.transaction_item
    ADD CONSTRAINT transaction_item_credit_item_id_fkey FOREIGN KEY (credit_item_id) REFERENCES public.account_item(id) ON DELETE CASCADE;
ALTER TABLE public.transaction_item
    ADD CONSTRAINT transaction_item_debit_item_id_fkey FOREIGN KEY (debit_item_id) REFERENCES public.account_item(id) ON DELETE CASCADE;

-- lot_item table
CREATE SEQUENCE public.lot_item_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.lot_item_id_seq OWNER TO ddl_user;
CREATE TABLE public.lot_item (
    id integer NOT NULL,
    lot_id integer NOT NULL,
    price numeric(19,6) NOT NULL,
    shares numeric(19,6) NOT NULL,
    action character varying(10) NOT NULL,
    transaction_id integer NOT NULL
);
ALTER TABLE public.lot_item OWNER TO ddl_user;
ALTER SEQUENCE public.lot_item_id_seq OWNED BY public.lot_item.id;
ALTER TABLE ONLY public.lot_item ALTER COLUMN id SET DEFAULT nextval('public.lot_item_id_seq'::regclass);
ALTER TABLE ONLY public.lot_item
    ADD CONSTRAINT lot_item_pkey PRIMARY KEY (id);
CREATE INDEX ix_lot_item_lot_id ON public.lot_item USING btree (lot_id);
CREATE INDEX ix_lot_item_transaction_id ON public.lot_item USING btree (transaction_id);
ALTER TABLE ONLY public.lot_item
    ADD CONSTRAINT fk_lot_item_lot FOREIGN KEY (lot_id) REFERENCES public.lot(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.lot_item
    ADD CONSTRAINT fk_lot_item_transaction FOREIGN KEY (transaction_id) REFERENCES public.transaction(id) ON DELETE CASCADE;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.lot_item TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.lot_item_id_seq TO app_user;
