ALTER TABLE ONLY public.lot_note
    DROP CONSTRAINT fk_lot_note_lot;
DROP INDEX ix_lot_note_lot_id;
ALTER TABLE public.lot_note RENAME COLUMN lot_id TO lot_ids;
ALTER TABLE public.lot_note ALTER COLUMN lot_ids
    TYPE integer[] USING ARRAY[lot_ids];
CREATE INDEX ix_lot_note_lot_ids
    ON public.lot_note USING gin(lot_ids);
