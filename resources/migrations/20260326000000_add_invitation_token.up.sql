ALTER TABLE public.invitation
    ADD COLUMN token character varying(64) UNIQUE;
