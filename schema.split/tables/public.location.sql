CREATE TABLE public.location (
    id bigint NOT NULL,
    name text NOT NULL,
    description jsonb NOT NULL
);

GRANT ALL ON TABLE public.location TO anonymous;
ALTER TABLE public.location ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.location
    ADD CONSTRAINT location_pkey PRIMARY KEY (id);

CREATE POLICY admin_all ON public.location TO administrator USING (true) WITH CHECK (true);
CREATE POLICY public_view ON public.location FOR SELECT TO anonymous USING (true);

