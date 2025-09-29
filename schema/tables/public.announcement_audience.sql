CREATE TABLE public.announcement_audience (
    id bigint NOT NULL,
    announcement_id bigint NOT NULL,
    cohort_id bigint,
    audience_role public.announcement_audience_role,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    CONSTRAINT announcement_audience_audience_check CHECK (((cohort_id IS NULL) <> (audience_role IS NULL)))
);

GRANT ALL ON TABLE public.announcement_audience TO anonymous;
ALTER TABLE public.announcement_audience ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.announcement_audience
    ADD CONSTRAINT announcement_audience_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.announcement_audience
    ADD CONSTRAINT announcement_audience_announcement_id_fkey FOREIGN KEY (announcement_id) REFERENCES public.upozorneni(up_id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.announcement_audience
    ADD CONSTRAINT announcement_audience_cohort_id_fkey FOREIGN KEY (cohort_id) REFERENCES public.cohort(id);
ALTER TABLE ONLY public.announcement_audience
    ADD CONSTRAINT announcement_audience_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;

CREATE POLICY admin_all ON public.announcement_audience TO administrator USING (true) WITH CHECK (true);
CREATE POLICY current_tenant ON public.announcement_audience AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY member_view ON public.announcement_audience FOR SELECT TO member USING (true);

CREATE UNIQUE INDEX announcement_audience_announcement_cohort_idx ON public.announcement_audience USING btree (announcement_id, cohort_id);
CREATE UNIQUE INDEX announcement_audience_announcement_role_idx ON public.announcement_audience USING btree (announcement_id, audience_role);
CREATE INDEX announcement_audience_tenant_idx ON public.announcement_audience USING btree (tenant_id);
