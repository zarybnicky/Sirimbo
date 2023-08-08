CREATE TABLE public.cohort_membership (
    cohort_id bigint NOT NULL,
    person_id bigint NOT NULL,
    since timestamp with time zone DEFAULT now() NOT NULL,
    until timestamp with time zone,
    active boolean DEFAULT true NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);

COMMENT ON TABLE public.cohort_membership IS '@omit create,update,delete
@simpleCollections only';

GRANT ALL ON TABLE public.cohort_membership TO anonymous;
ALTER TABLE public.cohort_membership ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.cohort_membership
    ADD CONSTRAINT cohort_membership_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.cohort_membership
    ADD CONSTRAINT cohort_membership_cohort_id_fkey FOREIGN KEY (cohort_id) REFERENCES public.skupiny(s_id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.cohort_membership
    ADD CONSTRAINT cohort_membership_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_all ON public.cohort_membership TO administrator USING (true);
CREATE POLICY view_visible_person ON public.cohort_membership FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.person
  WHERE (cohort_membership.person_id = person.id))));

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.cohort_membership FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

CREATE INDEX cohort_membership_active_idx ON public.cohort_membership USING btree (active);
CREATE INDEX cohort_membership_cohort_id_idx ON public.cohort_membership USING btree (cohort_id);
CREATE INDEX cohort_membership_person_id_idx ON public.cohort_membership USING btree (person_id);