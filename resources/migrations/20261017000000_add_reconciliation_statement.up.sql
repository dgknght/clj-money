ALTER TABLE public.reconciliation ADD COLUMN statement_id bigint;
CREATE INDEX ix_reconciliation_statement_id
    ON public.reconciliation USING btree (statement_id);
ALTER TABLE ONLY public.reconciliation
    ADD CONSTRAINT fk_reconciliation_statement
    FOREIGN KEY (statement_id) REFERENCES public.image(id) ON DELETE SET NULL;
