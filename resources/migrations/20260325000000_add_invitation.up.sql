CREATE SEQUENCE public.invitation_id_seq
    START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER SEQUENCE public.invitation_id_seq OWNER TO ddl_user;
CREATE TABLE public.invitation (
    id integer NOT NULL,
    user_id integer NOT NULL,
    recipient character varying(254) NOT NULL,
    note text,
    status character varying(20) NOT NULL DEFAULT 'unsent',
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);
ALTER TABLE public.invitation OWNER TO ddl_user;
ALTER SEQUENCE public.invitation_id_seq
    OWNED BY public.invitation.id;
ALTER TABLE ONLY public.invitation
    ALTER COLUMN id SET DEFAULT
    nextval('public.invitation_id_seq'::regclass);
ALTER TABLE ONLY public.invitation
    ADD CONSTRAINT invitation_pkey PRIMARY KEY (id);
CREATE INDEX ix_invitation_user_id
    ON public.invitation USING btree (user_id);
ALTER TABLE ONLY public.invitation
    ADD CONSTRAINT fk_invitation_user
    FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.invitation TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.invitation_id_seq TO app_user;
