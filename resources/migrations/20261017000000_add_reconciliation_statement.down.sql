ALTER TABLE public.reconciliation DROP CONSTRAINT fk_reconciliation_statement;
DROP INDEX public.ix_reconciliation_statement_id;
ALTER TABLE public.reconciliation DROP COLUMN statement_id;
