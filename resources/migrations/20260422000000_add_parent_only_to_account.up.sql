ALTER TABLE public.account
  ADD COLUMN parent_only boolean NOT NULL DEFAULT false;
