CREATE SEQUENCE public.cli_login_id_seq
    START WITH 1 INCREMENT BY 1 NO MINVALUE NO MAXVALUE CACHE 1;
ALTER SEQUENCE public.cli_login_id_seq OWNER TO ddl_user;
CREATE TABLE public.cli_login (
    id integer NOT NULL,
    device_code character varying(64) NOT NULL,
    user_code character varying(9) NOT NULL,
    user_id integer,
    expires_at timestamp with time zone NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);
ALTER TABLE public.cli_login OWNER TO ddl_user;
ALTER SEQUENCE public.cli_login_id_seq
    OWNED BY public.cli_login.id;
ALTER TABLE ONLY public.cli_login
    ALTER COLUMN id SET DEFAULT
    nextval('public.cli_login_id_seq'::regclass);
ALTER TABLE ONLY public.cli_login
    ADD CONSTRAINT cli_login_pkey PRIMARY KEY (id);
CREATE UNIQUE INDEX uk_cli_login_device_code
    ON public.cli_login USING btree (device_code);
CREATE UNIQUE INDEX uk_cli_login_user_code
    ON public.cli_login USING btree (user_code);
CREATE INDEX ix_cli_login_user_id
    ON public.cli_login USING btree (user_id);
ALTER TABLE ONLY public.cli_login
    ADD CONSTRAINT fk_cli_login_user
    FOREIGN KEY (user_id) REFERENCES public."user"(id) ON DELETE CASCADE;
GRANT SELECT,INSERT,DELETE,UPDATE ON TABLE public.cli_login TO app_user;
GRANT SELECT,UPDATE ON SEQUENCE public.cli_login_id_seq TO app_user;
