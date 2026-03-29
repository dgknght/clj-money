ALTER TABLE public.invitation
    DROP CONSTRAINT fk_invitation_user;
DROP INDEX ix_invitation_user_id;
ALTER TABLE public.invitation
    DROP COLUMN user_id;
ALTER TABLE public.invitation
    DROP CONSTRAINT fk_invitation_invited_by;
DROP INDEX ix_invitation_invited_by_id;
ALTER TABLE public.invitation
    RENAME COLUMN invited_by_id TO user_id;
ALTER TABLE public.invitation
    ADD CONSTRAINT fk_invitation_user
    FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE;
CREATE INDEX ix_invitation_user_id
    ON public.invitation USING btree (user_id);
