CREATE TABLE transaction_items (
    id integer NOT NULL,
    transaction_id integer NOT NULL,
    account_id integer NOT NULL,
    action character varying(10) NOT NULL,
    amount numeric(10,2) NOT NULL,
    balance numeric(10,2) NOT NULL,
    index integer NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    reconciliation_id integer,
    memo character varying(200),
    value numeric(10,2) NOT NULL
);

CREATE SEQUENCE transaction_items_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE transaction_items_id_seq OWNED BY transaction_items.id;

CREATE TABLE transactions (
    id integer NOT NULL,
    entity_id integer NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    description character varying(200) NOT NULL,
    memo character varying(200),
    transaction_date date NOT NULL
);

CREATE SEQUENCE transactions_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE transactions_id_seq OWNED BY transactions.id;

ALTER TABLE ONLY transaction_items ALTER COLUMN id SET DEFAULT nextval('transaction_items_id_seq'::regclass);

ALTER TABLE ONLY transactions ALTER COLUMN id SET DEFAULT nextval('transactions_id_seq'::regclass);

ALTER TABLE ONLY transaction_items
    ADD CONSTRAINT transaction_items_pkey PRIMARY KEY (id);

ALTER TABLE ONLY transactions
    ADD CONSTRAINT transactions_pkey PRIMARY KEY (id);

CREATE INDEX ix_transaction_items_index ON transaction_items USING btree (account_id, index);

CREATE INDEX ix_transaction_items_reconciliation ON transaction_items USING btree (reconciliation_id);

CREATE INDEX ix_transaction_items_transaction ON transaction_items USING btree (transaction_id);

ALTER TABLE ONLY transaction_items
    ADD CONSTRAINT fk_transaction_items_account FOREIGN KEY (account_id) REFERENCES accounts(id) ON DELETE CASCADE;

ALTER TABLE ONLY transaction_items
    ADD CONSTRAINT fk_transaction_items_reconciliation FOREIGN KEY (reconciliation_id) REFERENCES reconciliations(id) ON DELETE CASCADE;

ALTER TABLE ONLY transaction_items
    ADD CONSTRAINT fk_transaction_items_transaction FOREIGN KEY (transaction_id) REFERENCES transactions(id) ON DELETE CASCADE;

ALTER TABLE ONLY transactions
    ADD CONSTRAINT fk_transactions_entity FOREIGN KEY (entity_id) REFERENCES entities(id) ON DELETE CASCADE;
