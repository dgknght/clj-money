create table cached_prices (
  trade_date date not null,
  id uuid default public.gen_random_uuid() not null,
  symbol varchar(10) not null,
  exchange varchar(10) not null,
  price numeric(12,4) not null,
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  updated_at timestamp with time zone DEFAULT now() NOT NULL,
  constraint pk_cached_prices primary key (trade_date, id)
) partition by range(trade_date);

GRANT SELECT, INSERT, UPDATE, DELETE ON public.cached_prices TO app_user;
