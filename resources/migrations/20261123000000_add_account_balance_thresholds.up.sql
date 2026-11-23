ALTER TABLE public.account
  ADD COLUMN minimum_balance numeric(19,6),
  ADD COLUMN maximum_balance numeric(19,6);
