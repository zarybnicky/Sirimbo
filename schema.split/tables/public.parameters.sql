CREATE TABLE public.parameters (
    pa_name character varying(40) NOT NULL,
    pa_value text NOT NULL
);

GRANT ALL ON TABLE public.parameters TO anonymous;
ALTER TABLE public.parameters ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.parameters
    ADD CONSTRAINT idx_23816_primary PRIMARY KEY (pa_name);

CREATE POLICY admin_all ON public.parameters TO administrator USING (true) WITH CHECK (true);
CREATE POLICY all_view ON public.parameters FOR SELECT USING (true);

