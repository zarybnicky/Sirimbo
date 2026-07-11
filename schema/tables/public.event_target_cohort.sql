CREATE TABLE public.event_target_cohort (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    event_id bigint NOT NULL,
    cohort_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);

COMMENT ON TABLE public.event_target_cohort IS '@omit';

ALTER TABLE public.event_target_cohort ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.event_target_cohort
    ADD CONSTRAINT event_target_cohort_cohort_id_key UNIQUE (event_id, cohort_id);
ALTER TABLE ONLY public.event_target_cohort
    ADD CONSTRAINT event_target_cohort_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.event_target_cohort
    ADD CONSTRAINT event_target_cohort_cohort_fkey FOREIGN KEY (tenant_id, cohort_id) REFERENCES public.cohort(tenant_id, id) ON UPDATE CASCADE ON DELETE RESTRICT;
ALTER TABLE ONLY public.event_target_cohort
    ADD CONSTRAINT event_target_cohort_event_fkey FOREIGN KEY (tenant_id, event_id) REFERENCES public.event(tenant_id, id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_target_cohort
    ADD CONSTRAINT event_target_cohort_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event_target_cohort FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

CREATE INDEX event_target_cohort_cohort_id_idx ON public.event_target_cohort USING btree (cohort_id);
CREATE INDEX event_target_cohort_event_id_idx ON public.event_target_cohort USING btree (event_id);
CREATE INDEX event_target_cohort_tenant_id_id_idx ON public.event_target_cohort USING btree (tenant_id, id) INCLUDE (cohort_id);
CREATE INDEX event_target_cohort_tenant_id_idx ON public.event_target_cohort USING btree (tenant_id);
