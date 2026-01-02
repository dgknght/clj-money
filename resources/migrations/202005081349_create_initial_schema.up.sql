-- SET statement_timeout = 0;
-- SET lock_timeout = 0;
-- SET idle_in_transaction_session_timeout = 0;
-- SET transaction_timeout = 0;
-- SET client_encoding = 'UTF8';
-- SET standard_conforming_strings = on;
-- SELECT pg_catalog.set_config('search_path', '', false);
-- SET check_function_bodies = false;
-- SET xmloption = content;
-- SET client_min_messages = warning;
-- SET row_security = off;
CREATE EXTENSION IF NOT EXISTS pgcrypto WITH SCHEMA public;
COMMENT ON EXTENSION pgcrypto IS 'cryptographic functions';
SET default_tablespace = '';
SET default_table_access_method = heap;
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
CREATE SEQUENCE public.accounts_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.accounts_id_seq OWNER TO ddl_user;
ALTER SEQUENCE public.accounts_id_seq OWNED BY public.account.id;
CREATE TABLE public.attachment (
    id integer NOT NULL,
    caption character varying(255),
    image_id integer NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    transaction_id uuid NOT NULL,
    transaction_date date NOT NULL
);
ALTER TABLE public.attachment OWNER TO ddl_user;
CREATE SEQUENCE public.attachments_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.attachments_id_seq OWNER TO ddl_user;
ALTER SEQUENCE public.attachments_id_seq OWNED BY public.attachment.id;
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
CREATE SEQUENCE public.budget_items_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.budget_items_id_seq OWNER TO ddl_user;
ALTER SEQUENCE public.budget_items_id_seq OWNED BY public.budget_item.id;
CREATE SEQUENCE public.budgets_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.budgets_id_seq OWNER TO ddl_user;
ALTER SEQUENCE public.budgets_id_seq OWNED BY public.budget.id;
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
CREATE SEQUENCE public.commodities_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.commodities_id_seq OWNER TO ddl_user;
ALTER SEQUENCE public.commodities_id_seq OWNED BY public.commodity.id;
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
CREATE SEQUENCE public.entities_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.entities_id_seq OWNER TO ddl_user;
ALTER SEQUENCE public.entities_id_seq OWNED BY public.entity.id;
CREATE TABLE public."grant" (
    id integer NOT NULL,
    entity_id integer NOT NULL,
    user_id integer NOT NULL,
    permissions jsonb,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);
ALTER TABLE public."grant" OWNER TO ddl_user;
CREATE SEQUENCE public.grants_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.grants_id_seq OWNER TO ddl_user;
ALTER SEQUENCE public.grants_id_seq OWNED BY public."grant".id;
CREATE TABLE public.identity (
    id integer NOT NULL,
    user_id integer NOT NULL,
    provider character varying(20) NOT NULL,
    provider_id character varying(255) NOT NULL
);
ALTER TABLE public.identity OWNER TO ddl_user;
CREATE SEQUENCE public.identities_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.identities_id_seq OWNER TO ddl_user;
ALTER SEQUENCE public.identities_id_seq OWNED BY public.identity.id;
CREATE TABLE public.image (
    id integer NOT NULL,
    user_id integer NOT NULL,
    original_filename character varying(255) NOT NULL,
    uuid character(40) NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    content_type character varying(100) NOT NULL
);
ALTER TABLE public.image OWNER TO ddl_user;
CREATE SEQUENCE public.image_contents_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.image_contents_id_seq OWNER TO ddl_user;
ALTER SEQUENCE public.image_contents_id_seq OWNED BY public.image.id;
CREATE TABLE public.image_content (
    id integer DEFAULT nextval('public.image_contents_id_seq'::regclass) NOT NULL,
    uuid character(40) NOT NULL,
    content bytea NOT NULL
);
ALTER TABLE public.image_content OWNER TO ddl_user;
CREATE SEQUENCE public.images_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.images_id_seq OWNER TO ddl_user;
ALTER SEQUENCE public.images_id_seq OWNED BY public.image.id;
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
CREATE SEQUENCE public.imports_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.imports_id_seq OWNER TO ddl_user;
ALTER SEQUENCE public.imports_id_seq OWNED BY public.import.id;
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
CREATE TABLE public.lot_item (
    lot_id integer NOT NULL,
    price numeric(19,6) NOT NULL,
    shares numeric(19,6) NOT NULL,
    action character varying(10) NOT NULL,
    transaction_id uuid NOT NULL,
    transaction_date date NOT NULL,
    id integer NOT NULL
);
ALTER TABLE public.lot_item OWNER TO ddl_user;
CREATE SEQUENCE public.lot_items_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.lot_items_id_seq OWNER TO ddl_user;
ALTER SEQUENCE public.lot_items_id_seq OWNED BY public.lot_item.id;
CREATE SEQUENCE public.lots_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.lots_id_seq OWNER TO ddl_user;
ALTER SEQUENCE public.lots_id_seq OWNED BY public.lot.id;
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
CREATE TABLE public.reconciliation (
    end_of_period date NOT NULL,
    id uuid DEFAULT public.gen_random_uuid() NOT NULL,
    account_id integer NOT NULL,
    status character varying(20) NOT NULL,
    balance numeric(19,6) NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
)
PARTITION BY RANGE (end_of_period);
ALTER TABLE public.reconciliation OWNER TO ddl_user;
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
CREATE SEQUENCE public.scheduled_transaction_item_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.scheduled_transaction_item_id_seq OWNER TO ddl_user;
ALTER SEQUENCE public.scheduled_transaction_item_id_seq OWNED BY public.scheduled_transaction_item.id;
CREATE SEQUENCE public.scheduled_transactions_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.scheduled_transactions_id_seq OWNER TO ddl_user;
ALTER SEQUENCE public.scheduled_transactions_id_seq OWNED BY public.scheduled_transaction.id;
CREATE TABLE public.transaction (
    transaction_date date NOT NULL,
    id uuid DEFAULT public.gen_random_uuid() NOT NULL,
    entity_id integer NOT NULL,
    description character varying(200) NOT NULL,
    memo character varying(200),
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    value numeric(19,6) DEFAULT 0 NOT NULL,
    scheduled_transaction_id integer,
    attachment_count smallint DEFAULT 0 NOT NULL
)
PARTITION BY RANGE (transaction_date);
ALTER TABLE public.transaction OWNER TO ddl_user;
CREATE TABLE public.transaction_item (
    transaction_date date NOT NULL,
    id uuid DEFAULT public.gen_random_uuid() NOT NULL,
    transaction_id uuid NOT NULL,
    debit_account_id integer NOT NULL,
    debit_quantity numeric(19,6),
    debit_memo character varying(200),
    credit_account_id integer NOT NULL,
    credit_quantity numeric(19,6),
    credit_memo character varying(200),
    value numeric(19,6) NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
)
PARTITION BY RANGE (transaction_date);
ALTER TABLE public.transaction_item OWNER TO ddl_user;
CREATE TABLE public.account_item (
    transaction_date date NOT NULL,
    id uuid DEFAULT public.gen_random_uuid() NOT NULL,
    transaction_item_id uuid NOT NULL,
    account_id integer NOT NULL,
    index bigint NOT NULL,
    balance numeric(19,6) NOT NULL,
    quantity numeric(19,6) NOT NULL,
    reconciliation_id uuid,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
)
PARTITION BY RANGE (transaction_date);
ALTER TABLE public.account_item OWNER TO ddl_user;
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
CREATE SEQUENCE public.users_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.users_id_seq OWNER TO ddl_user;
ALTER SEQUENCE public.users_id_seq OWNED BY public."user".id;
ALTER TABLE ONLY public.account ALTER COLUMN id SET DEFAULT nextval('public.accounts_id_seq'::regclass);
ALTER TABLE ONLY public.attachment ALTER COLUMN id SET DEFAULT nextval('public.attachments_id_seq'::regclass);
ALTER TABLE ONLY public.budget ALTER COLUMN id SET DEFAULT nextval('public.budgets_id_seq'::regclass);
ALTER TABLE ONLY public.budget_item ALTER COLUMN id SET DEFAULT nextval('public.budget_items_id_seq'::regclass);
ALTER TABLE ONLY public.commodity ALTER COLUMN id SET DEFAULT nextval('public.commodities_id_seq'::regclass);
ALTER TABLE ONLY public.entity ALTER COLUMN id SET DEFAULT nextval('public.entities_id_seq'::regclass);
ALTER TABLE ONLY public."grant" ALTER COLUMN id SET DEFAULT nextval('public.grants_id_seq'::regclass);
ALTER TABLE ONLY public.identity ALTER COLUMN id SET DEFAULT nextval('public.identities_id_seq'::regclass);
ALTER TABLE ONLY public.image ALTER COLUMN id SET DEFAULT nextval('public.images_id_seq'::regclass);
ALTER TABLE ONLY public.import ALTER COLUMN id SET DEFAULT nextval('public.imports_id_seq'::regclass);
ALTER TABLE ONLY public.lot ALTER COLUMN id SET DEFAULT nextval('public.lots_id_seq'::regclass);
ALTER TABLE ONLY public.lot_item ALTER COLUMN id SET DEFAULT nextval('public.lot_items_id_seq'::regclass);
ALTER TABLE ONLY public.scheduled_transaction ALTER COLUMN id SET DEFAULT nextval('public.scheduled_transactions_id_seq'::regclass);
ALTER TABLE ONLY public.scheduled_transaction_item ALTER COLUMN id SET DEFAULT nextval('public.scheduled_transaction_item_id_seq'::regclass);
ALTER TABLE ONLY public."user" ALTER COLUMN id SET DEFAULT nextval('public.users_id_seq'::regclass);
ALTER TABLE ONLY public.account
    ADD CONSTRAINT accounts_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.attachment
    ADD CONSTRAINT attachments_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.budget_item
    ADD CONSTRAINT budget_items_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.budget
    ADD CONSTRAINT budgets_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.cached_price
    ADD CONSTRAINT pk_cached_prices PRIMARY KEY (trade_date, id);
ALTER TABLE ONLY public.commodity
    ADD CONSTRAINT commodities_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.entity
    ADD CONSTRAINT entities_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public."grant"
    ADD CONSTRAINT grants_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.identity
    ADD CONSTRAINT identities_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.image_content
    ADD CONSTRAINT image_contents_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.image
    ADD CONSTRAINT images_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.import
    ADD CONSTRAINT imports_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.lot_item
    ADD CONSTRAINT lot_items_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.lot
    ADD CONSTRAINT lots_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.price
    ADD CONSTRAINT prices_pkey PRIMARY KEY (trade_date, id);
ALTER TABLE ONLY public.reconciliation
    ADD CONSTRAINT reconciliations_pkey PRIMARY KEY (end_of_period, id);
ALTER TABLE ONLY public.scheduled_transaction_item
    ADD CONSTRAINT scheduled_transaction_item_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.scheduled_transaction
    ADD CONSTRAINT scheduled_transactions_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.transaction_item
    ADD CONSTRAINT transaction_item_pkey PRIMARY KEY (transaction_date, id);
ALTER TABLE ONLY public.account_item
    ADD CONSTRAINT account_item_pkey PRIMARY KEY (transaction_date, id);
ALTER TABLE ONLY public.transaction
    ADD CONSTRAINT transactions_pkey PRIMARY KEY (transaction_date, id);
ALTER TABLE ONLY public."user"
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);
CREATE INDEX ix_attachments_image_id ON public.attachment USING btree (image_id);
CREATE INDEX ix_grants_user ON public."grant" USING btree (user_id);
CREATE INDEX ix_identities_user_id ON public.identity USING btree (user_id);
CREATE INDEX ix_imports_user_id ON public.import USING btree (user_id);
CREATE INDEX ix_lots_account_id ON public.lot USING btree (account_id);
CREATE INDEX ix_lots_commodity_id ON public.lot USING btree (commodity_id);
CREATE INDEX ix_lots_transactions_lot_id ON public.lot_item USING btree (lot_id);
CREATE INDEX ix_lots_transactions_transaction_id ON public.lot_item USING btree (transaction_id);
CREATE INDEX ix_prices_trade_date_commodity_id ON ONLY public.price USING btree (trade_date, commodity_id);
CREATE INDEX ix_prices_trade_date_id ON ONLY public.price USING btree (trade_date, id);
CREATE INDEX ix_reconciliations_eop_account_id ON ONLY public.reconciliation USING btree (end_of_period, account_id);
CREATE INDEX ix_reconciliations_eop_id ON ONLY public.reconciliation USING btree (end_of_period, id);
CREATE INDEX ix_account_item_account_id ON ONLY public.account_item USING btree (account_id);
CREATE INDEX ix_transaction_item_transaction_date_id ON ONLY public.transaction_item USING btree (transaction_date, id);
CREATE INDEX ix_transaction_item_transaction_date_trx_id ON ONLY public.transaction_item USING btree (transaction_date, transaction_id);
CREATE INDEX ix_transactions_scheduled_transaction_id ON ONLY public.transaction USING btree (scheduled_transaction_id);
CREATE INDEX ix_transactions_transaction_date_entity_id ON ONLY public.transaction USING btree (transaction_date, entity_id);
CREATE INDEX ix_transactions_transaction_date_id ON ONLY public.transaction USING btree (transaction_date, id);
CREATE UNIQUE INDEX uk_accounts_name ON public.account USING btree (entity_id, parent_id, name);
CREATE UNIQUE INDEX uk_budget_items_account ON public.budget_item USING btree (budget_id, account_id);
CREATE UNIQUE INDEX uk_budgets_name ON public.budget USING btree (entity_id, name);
CREATE UNIQUE INDEX uk_commodities_name ON public.commodity USING btree (entity_id, exchange, name);
CREATE UNIQUE INDEX uk_commodities_symbol ON public.commodity USING btree (entity_id, exchange, symbol);
CREATE UNIQUE INDEX uk_entities_name ON public.entity USING btree (user_id, name);
CREATE UNIQUE INDEX uk_grants_entity_user ON public."grant" USING btree (entity_id, user_id);
CREATE UNIQUE INDEX uk_identities_provider_id ON public.identity USING btree (provider, provider_id);
CREATE UNIQUE INDEX uk_image_contents_uuid ON public.image_content USING btree (uuid);
CREATE UNIQUE INDEX uk_images_user_hash ON public.image USING btree (user_id, uuid);
CREATE UNIQUE INDEX uk_users_email ON public."user" USING btree (email);
CREATE INDEX uk_users_password_reset_token ON public."user" USING btree (password_reset_token);
ALTER TABLE ONLY public.account
    ADD CONSTRAINT fk_accounts_account FOREIGN KEY (parent_id) REFERENCES public.account(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.account
    ADD CONSTRAINT fk_accounts_entity FOREIGN KEY (entity_id) REFERENCES public.entity(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.attachment
    ADD CONSTRAINT fk_attachments_image FOREIGN KEY (image_id) REFERENCES public.image(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.attachment
    ADD CONSTRAINT fk_attachments_transaction FOREIGN KEY (transaction_date, transaction_id) REFERENCES public.transaction(transaction_date, id) ON DELETE CASCADE;
ALTER TABLE ONLY public.budget_item
    ADD CONSTRAINT fk_budget_items_account FOREIGN KEY (account_id) REFERENCES public.account(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.budget_item
    ADD CONSTRAINT fk_budget_items_budget FOREIGN KEY (budget_id) REFERENCES public.budget(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.budget
    ADD CONSTRAINT fk_budgets_entity FOREIGN KEY (entity_id) REFERENCES public.entity(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.commodity
    ADD CONSTRAINT fk_commodities_entity FOREIGN KEY (entity_id) REFERENCES public.entity(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.entity
    ADD CONSTRAINT fk_entities_user FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE;
ALTER TABLE ONLY public."grant"
    ADD CONSTRAINT fk_grants_entities FOREIGN KEY (entity_id) REFERENCES public.entity(id) ON DELETE CASCADE;
ALTER TABLE ONLY public."grant"
    ADD CONSTRAINT fk_grants_users FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.image
    ADD CONSTRAINT fk_images_user FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.import
    ADD CONSTRAINT fk_imports_user FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.lot
    ADD CONSTRAINT fk_lots_account FOREIGN KEY (account_id) REFERENCES public.account(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.lot
    ADD CONSTRAINT fk_lots_commodity FOREIGN KEY (commodity_id) REFERENCES public.commodity(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.lot_item
    ADD CONSTRAINT fk_lots_transactions_lot FOREIGN KEY (lot_id) REFERENCES public.lot(id) ON DELETE CASCADE;
ALTER TABLE public.price
    ADD CONSTRAINT fk_prices_commodity FOREIGN KEY (commodity_id) REFERENCES public.commodity(id) ON DELETE CASCADE;
ALTER TABLE public.reconciliation
    ADD CONSTRAINT fk_reconciliations_account FOREIGN KEY (account_id) REFERENCES public.account(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.scheduled_transaction_item
    ADD CONSTRAINT fk_scheduled_transaction_item_accounts FOREIGN KEY (account_id) REFERENCES public.account(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.scheduled_transaction_item
    ADD CONSTRAINT fk_scheduled_transaction_item_scheduled_transactions FOREIGN KEY (scheduled_transaction_id) REFERENCES public.scheduled_transaction(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.scheduled_transaction
    ADD CONSTRAINT fk_scheduled_transactions_entities FOREIGN KEY (entity_id) REFERENCES public.entity(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.lot_item
    ADD CONSTRAINT fk_transactions_lots_transactions FOREIGN KEY (transaction_date, transaction_id) REFERENCES public.transaction(transaction_date, id) ON DELETE CASCADE;
ALTER TABLE ONLY public.identity
    ADD CONSTRAINT identities_user_id_fkey FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE;
ALTER TABLE public.transaction_item
    ADD CONSTRAINT transaction_item_debit_account_id_fkey FOREIGN KEY (debit_account_id) REFERENCES public.account(id) ON DELETE CASCADE;
ALTER TABLE public.transaction_item
    ADD CONSTRAINT transaction_item_credit_account_id_fkey FOREIGN KEY (credit_account_id) REFERENCES public.account(id) ON DELETE CASCADE;
ALTER TABLE public.account_item
    ADD CONSTRAINT account_item_account_id_fkey FOREIGN KEY (account_id) REFERENCES public.account(id) ON DELETE CASCADE;
ALTER TABLE public.account_item
    ADD CONSTRAINT account_item_transaction_item_id_fkey FOREIGN KEY (transaction_date, transaction_item_id) REFERENCES public.transaction_item(transaction_date, id) ON DELETE CASCADE;
ALTER TABLE public.transaction
    ADD CONSTRAINT transactions_entity_id_fkey FOREIGN KEY (entity_id) REFERENCES public.entity(id) ON DELETE CASCADE;
ALTER TABLE public.transaction
    ADD CONSTRAINT transactions_scheduled_transaction_id_fkey FOREIGN KEY (scheduled_transaction_id) REFERENCES public.scheduled_transaction(id);
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.account TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.accounts_id_seq TO app_user;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.attachment TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.attachments_id_seq TO app_user;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.budget TO app_user;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.budget_item TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.budget_items_id_seq TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.budgets_id_seq TO app_user;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.cached_price TO app_user;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.commodity TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.commodities_id_seq TO app_user;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.entity TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.entities_id_seq TO app_user;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public."grant" TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.grants_id_seq TO app_user;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.identity TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.identities_id_seq TO app_user;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.image TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.image_contents_id_seq TO app_user;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.image_content TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.images_id_seq TO app_user;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.import TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.imports_id_seq TO app_user;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.lot TO app_user;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.lot_item TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.lot_items_id_seq TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.lots_id_seq TO app_user;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.price TO app_user;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.reconciliation TO app_user;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.scheduled_transaction TO app_user;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.scheduled_transaction_item TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.scheduled_transaction_item_id_seq TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.scheduled_transactions_id_seq TO app_user;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.transaction TO app_user;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.transaction_item TO app_user;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.account_item TO app_user;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public."user" TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.users_id_seq TO app_user;
