CREATE TABLE public.settings (
    name character varying(50) NOT NULL,
    value text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);
ALTER TABLE ONLY public.settings
    ADD CONSTRAINT settings_pkey PRIMARY KEY (name);

GRANT SELECT, INSERT, UPDATE, DELETE ON public.settings TO app_user;
