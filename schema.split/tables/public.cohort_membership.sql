CREATE TABLE public.cohort_membership (
    cohort_id bigint NOT NULL,
    person_id bigint NOT NULL,
    since timestamp with time zone DEFAULT now() NOT NULL,
    until timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[]'::text)) STORED NOT NULL
);

COMMENT ON TABLE public.cohort_membership IS '@omit delete
@simpleCollections only';
COMMENT ON COLUMN public.cohort_membership.active_range IS '@omit';

GRANT ALL ON TABLE public.cohort_membership TO anonymous;
ALTER TABLE public.cohort_membership ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.cohort_membership
    ADD CONSTRAINT cohort_membership_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.cohort_membership
    ADD CONSTRAINT cohort_membership_cohort_id_fkey FOREIGN KEY (cohort_id) REFERENCES public.skupiny(s_id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.cohort_membership
    ADD CONSTRAINT cohort_membership_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.cohort_membership
    ADD CONSTRAINT cohort_membership_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);

CREATE POLICY admin_all ON public.cohort_membership TO administrator USING (true);
CREATE POLICY view_visible_person ON public.cohort_membership FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.person
  WHERE (cohort_membership.person_id = person.id))));

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.cohort_membership FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

CREATE INDEX cohort_membership_cohort_id_idx ON public.cohort_membership USING btree (cohort_id);
CREATE INDEX cohort_membership_person_id_idx ON public.cohort_membership USING btree (person_id);
CREATE INDEX cohort_membership_range_idx ON public.cohort_membership USING gist (active_range, tenant_id, person_id);
CREATE INDEX idx_cm_tenant ON public.cohort_membership USING btree (tenant_id);