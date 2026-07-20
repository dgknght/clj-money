ALTER TABLE public.transaction_item
  DROP CONSTRAINT transaction_item_account_id_fkey;

ALTER TABLE public.transaction_item
  ADD CONSTRAINT transaction_item_account_id_fkey FOREIGN KEY (account_id) REFERENCES public.account(id) ON DELETE CASCADE;
