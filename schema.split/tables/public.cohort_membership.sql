CREATE TABLE public.cohort_membership (
    cohort_id bigint NOT NULL,
    person_id bigint NOT NULL,
    since timestamp with time zone DEFAULT now() NOT NULL,
    until timestamp with time zone,
    active boolean DEFAULT true NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);

COMMENT ON TABLE public.cohort_membership IS '@omit create,update,delete';

GRANT ALL ON TABLE public.cohort_membership TO anonymous;
ALTER TABLE public.cohort_membership ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.cohort_membership
    ADD CONSTRAINT cohort_membership_pkey PRIMARY KEY (cohort_id, person_id);
ALTER TABLE ONLY public.cohort_membership
    ADD CONSTRAINT cohort_membership_cohort_id_fkey FOREIGN KEY (cohort_id) REFERENCES public.skupiny(s_id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.cohort_membership
    ADD CONSTRAINT cohort_membership_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.cohort_membership FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

CREATE INDEX cohort_membership_cohort_id_idx ON public.cohort_membership USING btree (cohort_id);
CREATE INDEX cohort_membership_person_id_idx ON public.cohort_membership USING btree (person_id);