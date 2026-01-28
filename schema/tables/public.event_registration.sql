CREATE TABLE public.event_registration (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    event_id bigint NOT NULL,
    target_cohort_id bigint,
    couple_id bigint,
    person_id bigint,
    note text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT event_registration_check CHECK ((((couple_id IS NOT NULL) AND (person_id IS NULL)) OR ((couple_id IS NULL) AND (person_id IS NOT NULL))))
);

COMMENT ON TABLE public.event_registration IS '@omit update
@simpleCollections both';

GRANT ALL ON TABLE public.event_registration TO anonymous;
ALTER TABLE public.event_registration ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_tenant_id_id_event_id_ux UNIQUE (tenant_id, id, event_id);
ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_unique_event_person_couple_key UNIQUE NULLS NOT DISTINCT (event_id, person_id, couple_id);
ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_couple_id_fkey FOREIGN KEY (couple_id) REFERENCES public.couple(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_event_fkey FOREIGN KEY (tenant_id, event_id) REFERENCES public.event(tenant_id, id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_target_cohort_id_fkey FOREIGN KEY (target_cohort_id) REFERENCES public.event_target_cohort(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_all ON public.event_registration TO administrator USING (true);
CREATE POLICY delete_my ON public.event_registration FOR DELETE USING ((( SELECT public.event_is_registration_open(event.*) AS event_is_registration_open
   FROM public.event
  WHERE (event_registration.event_id = event.id)) AND ((person_id = ANY (( SELECT public.current_person_ids() AS current_person_ids)::bigint[])) OR (couple_id = ANY (( SELECT public.current_couple_ids() AS current_couple_ids)::bigint[])))));
CREATE POLICY trainer_same_tenant ON public.event_registration TO trainer USING (app_private.can_trainer_edit_event(event_id)) WITH CHECK (true);
CREATE POLICY update_my ON public.event_registration FOR UPDATE USING ((( SELECT public.event_is_registration_open(event.*) AS event_is_registration_open
   FROM public.event
  WHERE (event_registration.event_id = event.id)) AND ((person_id = ANY (( SELECT public.current_person_ids() AS current_person_ids)::bigint[])) OR (couple_id = ANY (( SELECT public.current_couple_ids() AS current_couple_ids)::bigint[])))));
CREATE POLICY view_visible_event ON public.event_registration FOR SELECT USING ((event_id IN ( SELECT event.id
   FROM public.event)));

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event_registration FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
CREATE TRIGGER _500_create_attendance AFTER INSERT ON public.event_registration FOR EACH ROW EXECUTE FUNCTION app_private.tg_event_registration__create_attendance();

CREATE INDEX event_registration_couple_id_idx ON public.event_registration USING btree (couple_id);
CREATE INDEX event_registration_event_id_idx ON public.event_registration USING btree (event_id);
CREATE INDEX event_registration_person_id_idx ON public.event_registration USING btree (person_id);
CREATE INDEX event_registration_target_cohort_id_idx ON public.event_registration USING btree (target_cohort_id);
CREATE INDEX event_registration_tenant_couple_event_idx ON public.event_registration USING btree (tenant_id, couple_id, event_id) WHERE (couple_id IS NOT NULL);
CREATE INDEX event_registration_tenant_event_id_idx ON public.event_registration USING btree (tenant_id, event_id);
CREATE INDEX event_registration_tenant_id_idx ON public.event_registration USING btree (tenant_id);
CREATE INDEX event_registration_tenant_person_event_idx ON public.event_registration USING btree (tenant_id, person_id, event_id) WHERE (person_id IS NOT NULL);
