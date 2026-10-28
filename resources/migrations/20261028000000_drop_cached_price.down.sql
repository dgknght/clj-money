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
