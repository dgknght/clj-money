DROP INDEX IF EXISTS public.uk_receipt_ingestion_entity_id_message_id;
ALTER TABLE public.receipt_ingestion DROP COLUMN message_id;
