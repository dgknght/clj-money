CREATE SEQUENCE scheduled_transactions_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

CREATE TABLE scheduled_transactions (
    id integer PRIMARY KEY NOT NULL DEFAULT nextval('scheduled_transactions_id_seq'),
    start_date date NOT NULL,
    end_date date,
    enabled boolean NOT NULL DEFAULT TRUE,
    last_occurrence date,
    date_spec jsonb NOT NULL,
    interval_type varchar(20) NOT NULL,
    interval_count integer NOT NULL,
    entity_id integer NOT NULL,
    description character varying(200) NOT NULL,
    memo character varying(200),
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT fk_scheduled_transactions_entities FOREIGN KEY (entity_id) REFERENCES entities (id) ON DELETE CASCADE
);
ALTER SEQUENCE scheduled_transactions_id_seq OWNED BY scheduled_transactions.id;

CREATE SEQUENCE scheduled_transaction_items_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;
CREATE TABLE scheduled_transaction_items (
    id integer PRIMARY KEY NOT NULL DEFAULT nextval('scheduled_transaction_items_id_seq'),
    scheduled_transaction_id integer NOT NULL,
    account_id integer NOT NULL,
    action character varying(10) NOT NULL,
    quantity numeric(12,4),
    memo character varying(200),
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT fk_scheduled_transaction_items_scheduled_transactions FOREIGN KEY (scheduled_transaction_id) REFERENCES scheduled_transactions (id) ON DELETE CASCADE,
    CONSTRAINT fk_scheduled_transaction_items_accounts FOREIGN KEY (account_id) REFERENCES accounts (id) ON DELETE CASCADE
);
ALTER SEQUENCE scheduled_transaction_items_id_seq OWNED BY scheduled_transaction_items.id;

ALTER TABLE transactions ADD COLUMN scheduled_transaction_id int references scheduled_transactions (id);
CREATE INDEX ix_transactions_scheduled_transaction_id ON transactions (scheduled_transaction_id);

GRANT SELECT, INSERT, UPDATE, DELETE ON public.scheduled_transactions TO app_user;
GRANT SELECT, INSERT, UPDATE, DELETE ON public.scheduled_transactions_id_seq TO app_user;
GRANT SELECT, INSERT, UPDATE, DELETE ON public.scheduled_transaction_items TO app_user;
GRANT SELECT, INSERT, UPDATE, DELETE ON public.scheduled_transaction_items_id_seq TO app_user;
