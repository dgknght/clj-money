ALTER TABLE public.commodity
  ADD COLUMN shares_owned numeric(19,6) NOT NULL DEFAULT 0;

UPDATE public.commodity c
SET shares_owned = (
  SELECT COALESCE(SUM(l.shares_owned), 0)
  FROM public.lot l
  WHERE l.commodity_id = c.id
    AND l.shares_owned > 0
);
