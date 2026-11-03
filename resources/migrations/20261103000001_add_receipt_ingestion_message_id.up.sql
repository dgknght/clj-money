ALTER TABLE public.receipt_ingestion ADD COLUMN message_id character varying(998);
CREATE UNIQUE INDEX uk_receipt_ingestion_entity_id_message_id
    ON public.receipt_ingestion USING btree (entity_id, message_id)
    WHERE message_id IS NOT NULL;
