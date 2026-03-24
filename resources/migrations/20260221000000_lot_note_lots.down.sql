DROP INDEX ix_lot_note_lot_ids;
ALTER TABLE public.lot_note ALTER COLUMN lot_ids
    TYPE integer USING lot_ids[1];
ALTER TABLE public.lot_note RENAME COLUMN lot_ids TO lot_id;
ALTER TABLE ONLY public.lot_note
    ADD CONSTRAINT fk_lot_note_lot
    FOREIGN KEY (lot_id) REFERENCES public.lot(id) ON DELETE CASCADE;
CREATE INDEX ix_lot_note_lot_id
    ON public.lot_note USING btree (lot_id);
