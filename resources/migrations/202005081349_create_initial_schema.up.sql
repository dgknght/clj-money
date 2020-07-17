CREATE EXTENSION IF NOT EXISTS pgcrypto WITH SCHEMA public;
CREATE TABLE public.accounts (
    id integer NOT NULL,
    name character varying(255) NOT NULL,
    type character varying(20) NOT NULL,
    entity_id integer NOT NULL,
    parent_id integer,
    quantity numeric(12,4) NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    commodity_id integer NOT NULL,
    tags text[],
    value numeric(12,4),
    earliest_transaction_date date,
    latest_transaction_date date,
    hidden boolean DEFAULT false
);
CREATE SEQUENCE public.accounts_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.accounts_id_seq OWNED BY public.accounts.id;
CREATE TABLE public.attachments (
    id integer NOT NULL,
    caption character varying(255) NOT NULL,
    image_id integer NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    transaction_id uuid NOT NULL,
    transaction_date date NOT NULL
);
CREATE SEQUENCE public.attachments_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.attachments_id_seq OWNED BY public.attachments.id;
CREATE TABLE public.budget_items (
    id integer NOT NULL,
    budget_id integer NOT NULL,
    account_id integer NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    periods numeric(12,4)[] NOT NULL
);
CREATE SEQUENCE public.budget_items_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.budget_items_id_seq OWNED BY public.budget_items.id;
CREATE TABLE public.budgets (
    id integer NOT NULL,
    entity_id integer NOT NULL,
    name character varying(50) NOT NULL,
    period character varying(20) NOT NULL,
    period_count smallint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    start_date date NOT NULL,
    end_date date NOT NULL
);
CREATE SEQUENCE public.budgets_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.budgets_id_seq OWNED BY public.budgets.id;
CREATE TABLE public.commodities (
    id integer NOT NULL,
    entity_id integer NOT NULL,
    name character varying(100) NOT NULL,
    symbol character varying(10) NOT NULL,
    exchange character varying(10),
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    type character varying(50) NOT NULL,
    earliest_price date,
    latest_price date
);
CREATE SEQUENCE public.commodities_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.commodities_id_seq OWNED BY public.commodities.id;
CREATE TABLE public.entities (
    id integer NOT NULL,
    user_id integer NOT NULL,
    name character varying(100) NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    settings text
);
CREATE SEQUENCE public.entities_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.entities_id_seq OWNED BY public.entities.id;
CREATE TABLE public.grants (
    id integer NOT NULL,
    entity_id integer NOT NULL,
    user_id integer NOT NULL,
    permissions text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);
CREATE SEQUENCE public.grants_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.grants_id_seq OWNED BY public.grants.id;
CREATE TABLE public.identities (
    id integer NOT NULL,
    user_id integer NOT NULL,
    provider character varying(20) NOT NULL,
    provider_id character varying(255) NOT NULL
);
CREATE SEQUENCE public.identities_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.identities_id_seq OWNED BY public.identities.id;
CREATE TABLE public.imports (
    id integer NOT NULL,
    user_id integer NOT NULL,
    entity_name character varying(100) NOT NULL,
    progress text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    image_ids integer[] NOT NULL
);
CREATE SEQUENCE public.imports_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.imports_id_seq OWNED BY public.imports.id;
CREATE TABLE public.images (
    id integer DEFAULT nextval('public.imports_id_seq'::regclass) NOT NULL,
    user_id integer NOT NULL,
    original_filename character varying(255) NOT NULL,
    body_hash character(40) NOT NULL,
    body bytea NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    content_type character varying(100) NOT NULL
);
CREATE SEQUENCE public.images_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.images_id_seq OWNED BY public.images.id;
CREATE TABLE public.lots (
    id integer NOT NULL,
    commodity_id integer NOT NULL,
    account_id integer NOT NULL,
    shares_purchased numeric(10,4) NOT NULL,
    shares_owned numeric(10,4) DEFAULT 0 NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    purchase_price numeric(12,4) NOT NULL,
    purchase_date date NOT NULL
);
CREATE SEQUENCE public.lots_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.lots_id_seq OWNED BY public.lots.id;
CREATE TABLE public.lots_transactions (
    lot_id integer NOT NULL,
    price numeric(12,4) NOT NULL,
    shares numeric(10,4) NOT NULL,
    lot_action character varying(10) NOT NULL,
    transaction_id uuid NOT NULL,
    transaction_date date NOT NULL
);
CREATE TABLE public.prices(
    trade_date date NOT NULL,
    id uuid DEFAULT public.gen_random_uuid() NOT NULL,
    commodity_id integer NOT NULL,
    price numeric(12,4) NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
) PARTITION BY RANGE (trade_date);
CREATE TABLE public.reconciliations (
    end_of_period date NOT NULL,
    id uuid DEFAULT public.gen_random_uuid() NOT NULL,
    account_id integer NOT NULL,
    status character varying(20) NOT NULL,
    balance numeric(10,2) NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
) PARTITION BY RANGE(end_of_period);
CREATE TABLE public.settings (
    name character varying(50) NOT NULL,
    value text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);
CREATE TABLE public.transaction_items (
    transaction_date date NOT NULL,
    id uuid DEFAULT public.gen_random_uuid() NOT NULL,
    transaction_id uuid NOT NULL,
    account_id integer NOT NULL,
    action character varying(10) NOT NULL,
    quantity numeric(12,4),
    value numeric(12,4),
    balance numeric(12,4),
    memo character varying(200),
    index bigint NOT NULL,
    reconciliation_id uuid,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    negative boolean
) PARTITION BY RANGE (transaction_date);
CREATE TABLE public.transactions (
    transaction_date date NOT NULL,
    id uuid DEFAULT public.gen_random_uuid() NOT NULL,
    entity_id integer NOT NULL,
    description character varying(200) NOT NULL,
    memo character varying(200),
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    value numeric(12,4) DEFAULT 0 NOT NULL
) PARTITION BY RANGE (transaction_date);
CREATE TABLE public.users (
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
CREATE SEQUENCE public.users_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
ALTER SEQUENCE public.users_id_seq OWNED BY public.users.id;

ALTER TABLE ONLY public.accounts ALTER COLUMN id SET DEFAULT nextval('public.accounts_id_seq'::regclass);
ALTER TABLE ONLY public.attachments ALTER COLUMN id SET DEFAULT nextval('public.attachments_id_seq'::regclass);
ALTER TABLE ONLY public.budget_items ALTER COLUMN id SET DEFAULT nextval('public.budget_items_id_seq'::regclass);
ALTER TABLE ONLY public.budgets ALTER COLUMN id SET DEFAULT nextval('public.budgets_id_seq'::regclass);
ALTER TABLE ONLY public.commodities ALTER COLUMN id SET DEFAULT nextval('public.commodities_id_seq'::regclass);
ALTER TABLE ONLY public.entities ALTER COLUMN id SET DEFAULT nextval('public.entities_id_seq'::regclass);
ALTER TABLE ONLY public.grants ALTER COLUMN id SET DEFAULT nextval('public.grants_id_seq'::regclass);
ALTER TABLE ONLY public.identities ALTER COLUMN id SET DEFAULT nextval('public.identities_id_seq'::regclass);
ALTER TABLE ONLY public.imports ALTER COLUMN id SET DEFAULT nextval('public.imports_id_seq'::regclass);
ALTER TABLE ONLY public.lots ALTER COLUMN id SET DEFAULT nextval('public.lots_id_seq'::regclass);
ALTER TABLE ONLY public.users ALTER COLUMN id SET DEFAULT nextval('public.users_id_seq'::regclass);
ALTER TABLE ONLY public.accounts
    ADD CONSTRAINT accounts_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.attachments
    ADD CONSTRAINT attachments_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.budget_items
    ADD CONSTRAINT budget_items_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.budgets
    ADD CONSTRAINT budgets_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.commodities
    ADD CONSTRAINT commodities_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.entities
    ADD CONSTRAINT entities_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.grants
    ADD CONSTRAINT grants_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.identities
    ADD CONSTRAINT identities_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.images
    ADD CONSTRAINT images_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.imports
    ADD CONSTRAINT imports_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.lots
    ADD CONSTRAINT lots_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.prices
    ADD CONSTRAINT prices_pkey PRIMARY KEY (trade_date, id);
ALTER TABLE ONLY public.reconciliations
    ADD CONSTRAINT reconciliations_pkey PRIMARY KEY (end_of_period, id);
ALTER TABLE ONLY public.settings
    ADD CONSTRAINT settings_pkey PRIMARY KEY (name);
ALTER TABLE ONLY public.transaction_items
    ADD CONSTRAINT transaction_items_pkey PRIMARY KEY (transaction_date, id);
ALTER TABLE ONLY public.transactions
    ADD CONSTRAINT transactions_pkey PRIMARY KEY (transaction_date, id);
ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);
CREATE INDEX ix_attachments_image_id ON public.attachments USING btree (image_id);
CREATE INDEX ix_grants_user ON public.grants USING btree (user_id);
CREATE INDEX ix_identities_user_id ON public.identities USING btree (user_id);
CREATE INDEX ix_imports_user_id ON public.imports USING btree (user_id);
CREATE INDEX ix_lots_account_id ON public.lots USING btree (account_id);
CREATE INDEX ix_lots_commodity_id ON public.lots USING btree (commodity_id);
CREATE UNIQUE INDEX uk_accounts_name ON public.accounts USING btree (entity_id, parent_id, name);
CREATE UNIQUE INDEX uk_budget_items_account ON public.budget_items USING btree (budget_id, account_id);
CREATE UNIQUE INDEX uk_budgets_name ON public.budgets USING btree (entity_id, name);
CREATE UNIQUE INDEX uk_commodities_name ON public.commodities USING btree (entity_id, exchange, name);
CREATE UNIQUE INDEX uk_commodities_symbol ON public.commodities USING btree (entity_id, exchange, symbol);
CREATE UNIQUE INDEX uk_entities_name ON public.entities USING btree (user_id, name);
CREATE UNIQUE INDEX uk_grants_entity_user ON public.grants USING btree (entity_id, user_id);
CREATE UNIQUE INDEX uk_identities_provider_id ON public.identities USING btree (provider, provider_id);
CREATE UNIQUE INDEX uk_images_user_hash ON public.images USING btree (user_id, body_hash);
CREATE INDEX ix_prices_trade_date_id ON public.prices USING btree (trade_date, id);
CREATE INDEX ix_prices_trade_date_commodity_id ON public.prices USING btree (trade_date, commodity_id);
CREATE INDEX ix_reconciliations_eop_id ON public.reconciliations USING btree (end_of_period, id);
CREATE INDEX ix_reconciliations_eop_account_id ON public.reconciliations USING btree (end_of_period, account_id);
CREATE INDEX ix_transaction_items_transaction_date_id ON public.transaction_items USING btree (transaction_date, id);
CREATE INDEX ix_transaction_items_transaction_date_trx_id ON public.transaction_items USING btree (transaction_date, transaction_id);
CREATE INDEX ix_transaction_items_transaction_date_account_id ON public.transaction_items USING btree (transaction_date, account_id);
CREATE INDEX ix_transactions_transaction_date_id ON public.transactions USING btree (transaction_date, id);
CREATE INDEX ix_transactions_transaction_date_entity_id ON public.transactions USING btree (transaction_date, entity_id);
CREATE UNIQUE INDEX uk_users_email ON public.users USING btree (email);
CREATE INDEX uk_users_password_reset_token ON public.users USING btree (password_reset_token);
ALTER TABLE ONLY public.accounts
    ADD CONSTRAINT fk_accounts_account FOREIGN KEY (parent_id) REFERENCES public.accounts(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.accounts
    ADD CONSTRAINT fk_accounts_entity FOREIGN KEY (entity_id) REFERENCES public.entities(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.attachments
    ADD CONSTRAINT fk_attachments_image FOREIGN KEY (image_id) REFERENCES public.images(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.budget_items
    ADD CONSTRAINT fk_budget_items_account FOREIGN KEY (account_id) REFERENCES public.accounts(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.budget_items
    ADD CONSTRAINT fk_budget_items_budget FOREIGN KEY (budget_id) REFERENCES public.budgets(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.budgets
    ADD CONSTRAINT fk_budgets_entity FOREIGN KEY (entity_id) REFERENCES public.entities(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.commodities
    ADD CONSTRAINT fk_commodities_entity FOREIGN KEY (entity_id) REFERENCES public.entities(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.entities
    ADD CONSTRAINT fk_entities_user FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.grants
    ADD CONSTRAINT fk_grants_entities FOREIGN KEY (entity_id) REFERENCES public.entities(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.grants
    ADD CONSTRAINT fk_grants_users FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.images
    ADD CONSTRAINT fk_images_user FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.imports
    ADD CONSTRAINT fk_imports_user FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.lots
    ADD CONSTRAINT fk_lots_account FOREIGN KEY (account_id) REFERENCES public.accounts(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.lots
    ADD CONSTRAINT fk_lots_commodity FOREIGN KEY (commodity_id) REFERENCES public.commodities(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.lots_transactions
    ADD CONSTRAINT fk_lots_transactions_lot FOREIGN KEY (lot_id) REFERENCES public.lots(id) ON DELETE CASCADE;
ALTER TABLE public.prices
    ADD CONSTRAINT fk_prices_commodity FOREIGN KEY (commodity_id) REFERENCES public.commodities(id) ON DELETE CASCADE;
ALTER TABLE public.reconciliations
    ADD CONSTRAINT fk_reconciliations_account FOREIGN KEY (account_id) REFERENCES public.accounts(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.identities
    ADD CONSTRAINT identities_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;
ALTER TABLE public.transaction_items
    ADD CONSTRAINT transaction_items_base_account_id_fkey FOREIGN KEY (account_id) REFERENCES public.accounts(id) ON DELETE CASCADE;
ALTER TABLE public.transactions
    ADD CONSTRAINT transactions_base_entity_id_fkey FOREIGN KEY (entity_id) REFERENCES public.entities(id) ON DELETE CASCADE;
