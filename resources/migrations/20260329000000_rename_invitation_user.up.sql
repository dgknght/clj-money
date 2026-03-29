ALTER TABLE public.invitation
    DROP CONSTRAINT fk_invitation_user;
DROP INDEX ix_invitation_user_id;
ALTER TABLE public.invitation
    RENAME COLUMN user_id TO invited_by_id;
ALTER TABLE public.invitation
    ADD CONSTRAINT fk_invitation_invited_by
    FOREIGN KEY (invited_by_id) REFERENCES public."user"(id) ON DELETE CASCADE;
CREATE INDEX ix_invitation_invited_by_id
    ON public.invitation USING btree (invited_by_id);
ALTER TABLE public.invitation
    ADD COLUMN user_id integer;
ALTER TABLE public.invitation
    ADD CONSTRAINT fk_invitation_user
    FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE SET NULL;
CREATE INDEX ix_invitation_user_id
    ON public.invitation USING btree (user_id);
