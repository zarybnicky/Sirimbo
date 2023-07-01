CREATE TABLE public.tenant_person (
    tenant_id bigint NOT NULL,
    person_id bigint NOT NULL
);

GRANT ALL ON TABLE public.tenant_person TO anonymous;
ALTER TABLE public.tenant_person ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.tenant_person
    ADD CONSTRAINT tenant_person_pkey PRIMARY KEY (tenant_id, person_id);
ALTER TABLE ONLY public.tenant_person
    ADD CONSTRAINT tenant_person_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id);
ALTER TABLE ONLY public.tenant_person
    ADD CONSTRAINT tenant_person_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);

CREATE POLICY admin_all ON public.tenant_person TO administrator USING (true) WITH CHECK (true);
CREATE POLICY my_tenant ON public.tenant_person AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));
CREATE POLICY public_view ON public.tenant_person FOR SELECT TO anonymous USING (true);

CREATE INDEX person_id ON public.tenant_person USING btree (person_id);