ALTER TABLE public."user"
  ADD COLUMN roles text[] NOT NULL DEFAULT '{user}';
