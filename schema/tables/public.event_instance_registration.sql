CREATE TABLE public.event_instance_registration (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    instance_id bigint NOT NULL,
    event_id bigint,
    parent_registration_id bigint,
    couple_id bigint,
    person_id bigint,
    target_cohort_id bigint,
    legacy_registration_id bigint,
    status public.attendance_type,
    note text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT event_instance_registration_shape CHECK (
CASE
    WHEN (parent_registration_id IS NULL) THEN (((couple_id IS NOT NULL) AND (person_id IS NULL)) OR ((couple_id IS NULL) AND (person_id IS NOT NULL)))
    ELSE ((person_id IS NOT NULL) AND (couple_id IS NULL))
END)
);

COMMENT ON TABLE public.event_instance_registration IS '@omit';

GRANT ALL ON TABLE public.event_instance_registration TO anonymous;
ALTER TABLE public.event_instance_registration ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.event_instance_registration
    ADD CONSTRAINT event_instance_registration_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.event_instance_registration
    ADD CONSTRAINT event_instance_registration_couple_id_fkey FOREIGN KEY (couple_id) REFERENCES public.couple(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_instance_registration
    ADD CONSTRAINT event_instance_registration_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.event(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_instance_registration
    ADD CONSTRAINT event_instance_registration_instance_id_fkey FOREIGN KEY (instance_id) REFERENCES public.event_instance(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_instance_registration
    ADD CONSTRAINT event_instance_registration_parent_registration_id_fkey FOREIGN KEY (parent_registration_id) REFERENCES public.event_instance_registration(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_instance_registration
    ADD CONSTRAINT event_instance_registration_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_instance_registration
    ADD CONSTRAINT event_instance_registration_target_cohort_id_fkey FOREIGN KEY (target_cohort_id) REFERENCES public.event_target_cohort(id) ON UPDATE CASCADE ON DELETE RESTRICT;
ALTER TABLE ONLY public.event_instance_registration
    ADD CONSTRAINT event_instance_registration_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_all ON public.event_instance_registration TO administrator USING (true);
CREATE POLICY current_tenant ON public.event_instance_registration AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY delete_my ON public.event_instance_registration FOR DELETE USING (((person_id = ANY (( SELECT public.current_person_ids() AS current_person_ids)::bigint[])) OR (couple_id = ANY (( SELECT public.current_couple_ids() AS current_couple_ids)::bigint[]))));
CREATE POLICY insert_my ON public.event_instance_registration FOR INSERT WITH CHECK (((person_id = ANY (( SELECT public.current_person_ids() AS current_person_ids)::bigint[])) OR (couple_id = ANY (( SELECT public.current_couple_ids() AS current_couple_ids)::bigint[]))));
CREATE POLICY trainer_same_tenant ON public.event_instance_registration TO trainer USING (app_private.can_trainer_edit_instance(instance_id)) WITH CHECK (true);
CREATE POLICY view_visible_instance ON public.event_instance_registration FOR SELECT USING ((instance_id IN ( SELECT event_instance.id
   FROM public.event_instance)));

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event_instance_registration FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

CREATE UNIQUE INDEX event_instance_registration_bridge_key ON public.event_instance_registration USING btree (legacy_registration_id, instance_id, COALESCE(person_id, ('-1'::integer)::bigint));
CREATE INDEX event_instance_registration_couple_id_idx ON public.event_instance_registration USING btree (couple_id);
CREATE INDEX event_instance_registration_event_id_idx ON public.event_instance_registration USING btree (event_id);
CREATE INDEX event_instance_registration_instance_id_idx ON public.event_instance_registration USING btree (instance_id);
CREATE INDEX event_instance_registration_legacy_registration_id_idx ON public.event_instance_registration USING btree (legacy_registration_id);
CREATE INDEX event_instance_registration_parent_id_idx ON public.event_instance_registration USING btree (parent_registration_id);
CREATE INDEX event_instance_registration_person_id_idx ON public.event_instance_registration USING btree (person_id);
CREATE UNIQUE INDEX event_instance_registration_person_key ON public.event_instance_registration USING btree (instance_id, person_id) WHERE ((person_id IS NOT NULL) AND (legacy_registration_id IS NULL));
CREATE INDEX event_instance_registration_target_cohort_id_idx ON public.event_instance_registration USING btree (target_cohort_id);
CREATE INDEX event_instance_registration_tenant_id_idx ON public.event_instance_registration USING btree (tenant_id);
CREATE UNIQUE INDEX event_instance_registration_unit_key ON public.event_instance_registration USING btree (instance_id, couple_id, person_id) NULLS NOT DISTINCT WHERE (parent_registration_id IS NULL);
