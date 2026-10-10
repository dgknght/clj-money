ALTER TABLE public.transaction ADD COLUMN source character varying(20);
ALTER TABLE public.transaction ADD COLUMN review_status character varying(20);

ALTER TABLE public.receipt_ingestion ADD COLUMN transaction_id bigint;
ALTER TABLE public.receipt_ingestion ADD COLUMN rejection_reason text;
CREATE INDEX ix_receipt_ingestion_transaction_id
    ON public.receipt_ingestion USING btree (transaction_id);
ALTER TABLE ONLY public.receipt_ingestion
    ADD CONSTRAINT fk_receipt_ingestion_transaction
    FOREIGN KEY (transaction_id) REFERENCES public.transaction(id) ON DELETE SET NULL;
