ALTER TABLE public.account ADD COLUMN payment_account_id integer;

ALTER TABLE ONLY public.account
    ADD CONSTRAINT fk_account_payment_account FOREIGN KEY (payment_account_id) REFERENCES public.account(id) ON DELETE SET NULL;
