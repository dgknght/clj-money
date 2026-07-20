CREATE FUNCTION public.delete_orphaned_lot_notes()
    RETURNS trigger
    LANGUAGE plpgsql
AS $$
BEGIN
    DELETE FROM lot_note WHERE OLD.id = ANY(lot_ids);
    RETURN OLD;
END;
$$;
ALTER FUNCTION public.delete_orphaned_lot_notes() OWNER TO ddl_user;
CREATE TRIGGER trg_delete_orphaned_lot_notes
    BEFORE DELETE ON public.lot
    FOR EACH ROW
    EXECUTE FUNCTION public.delete_orphaned_lot_notes();
