ALTER TABLE public.receipt_ingestion DROP CONSTRAINT fk_receipt_ingestion_transaction;
DROP INDEX public.ix_receipt_ingestion_transaction_id;
ALTER TABLE public.receipt_ingestion DROP COLUMN rejection_reason;
ALTER TABLE public.receipt_ingestion DROP COLUMN transaction_id;

ALTER TABLE public.transaction DROP COLUMN review_status;
ALTER TABLE public.transaction DROP COLUMN source;
