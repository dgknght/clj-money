ALTER TABLE public.account DROP CONSTRAINT fk_account_payment_account;

ALTER TABLE public.account DROP COLUMN payment_account_id;
