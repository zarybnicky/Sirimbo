CREATE TABLE public.event_instance_registration (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    instance_id bigint NOT NULL,
    parent_registration_id bigint,
    couple_id bigint,
    person_id bigint,
    target_cohort_id bigint,
    legacy_registration_id bigint,
    status public.attendance_type,
    note text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    attendance_created_at timestamp with time zone,
    attendance_updated_at timestamp with time zone,
    attendance_note text,
    registration_status public.event_instance_registration_status DEFAULT 'active'::public.event_instance_registration_status NOT NULL,
    source public.event_registration_source,
    CONSTRAINT event_instance_registration_attendance_state CHECK (((person_id IS NULL) OR ((status IS NOT NULL) AND (attendance_created_at IS NOT NULL) AND (attendance_updated_at IS NOT NULL)))),
    CONSTRAINT event_instance_registration_shape CHECK (
CASE
    WHEN (parent_registration_id IS NULL) THEN (((couple_id IS NOT NULL) AND (person_id IS NULL)) OR ((couple_id IS NULL) AND (person_id IS NOT NULL)))
    ELSE ((person_id IS NOT NULL) AND (couple_id IS NULL))
END)
);

COMMENT ON TABLE public.event_instance_registration IS '@omit create,update,delete
@simpleCollections both';
COMMENT ON COLUMN public.event_instance_registration.legacy_registration_id IS '@omit';

GRANT ALL ON TABLE public.event_instance_registration TO anonymous;
ALTER TABLE public.event_instance_registration ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.event_instance_registration
    ADD CONSTRAINT event_instance_registration_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.event_instance_registration
    ADD CONSTRAINT event_instance_registration_couple_id_fkey FOREIGN KEY (couple_id) REFERENCES public.couple(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_instance_registration
    ADD CONSTRAINT event_instance_registration_instance_id_fkey FOREIGN KEY (instance_id) REFERENCES public.event_instance(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_instance_registration
    ADD CONSTRAINT event_instance_registration_parent_registration_id_fkey FOREIGN KEY (parent_registration_id) REFERENCES public.event_instance_registration(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_instance_registration
    ADD CONSTRAINT event_instance_registration_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_instance_registration
    ADD CONSTRAINT event_instance_registration_target_cohort_id_fkey FOREIGN KEY (target_cohort_id) REFERENCES public.cohort(id) ON UPDATE CASCADE ON DELETE RESTRICT;
ALTER TABLE ONLY public.event_instance_registration
    ADD CONSTRAINT event_instance_registration_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_all ON public.event_instance_registration TO administrator USING (true);
CREATE POLICY current_tenant ON public.event_instance_registration AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY delete_my ON public.event_instance_registration FOR DELETE USING (((person_id = ANY (( SELECT public.current_person_ids() AS current_person_ids)::bigint[])) OR (couple_id = ANY (( SELECT public.current_couple_ids() AS current_couple_ids)::bigint[]))));
CREATE POLICY insert_my ON public.event_instance_registration FOR INSERT WITH CHECK (((person_id = ANY (( SELECT public.current_person_ids() AS current_person_ids)::bigint[])) OR (couple_id = ANY (( SELECT public.current_couple_ids() AS current_couple_ids)::bigint[]))));
CREATE POLICY trainer_insert ON public.event_instance_registration FOR INSERT TO trainer WITH CHECK (app_private.can_trainer_edit_instance(instance_id));
CREATE POLICY trainer_update ON public.event_instance_registration FOR UPDATE TO trainer USING (app_private.can_trainer_edit_instance(instance_id)) WITH CHECK (app_private.can_trainer_edit_instance(instance_id));
CREATE POLICY view_visible_instance ON public.event_instance_registration FOR SELECT USING ((instance_id IN ( SELECT event_instance.id
   FROM public.event_instance)));

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE OF tenant_id, instance_id, parent_registration_id, couple_id, person_id, target_cohort_id, status, note, attendance_note, registration_status, source, created_at, updated_at ON public.event_instance_registration FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
CREATE TRIGGER _200_eir_attendance_timestamps BEFORE INSERT OR UPDATE OF person_id, status, attendance_note ON public.event_instance_registration FOR EACH ROW EXECUTE FUNCTION app_private.tg_eir__attendance_timestamps();
CREATE TRIGGER _500_eir_refresh_stats_del AFTER DELETE ON public.event_instance_registration REFERENCING OLD TABLE AS deleted_rows FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_eir__refresh_stats_stmt();
CREATE TRIGGER _500_eir_refresh_stats_ins AFTER INSERT ON public.event_instance_registration REFERENCING NEW TABLE AS changed_rows FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_eir__refresh_stats_stmt();
CREATE TRIGGER _500_eir_refresh_stats_upd AFTER UPDATE ON public.event_instance_registration REFERENCING OLD TABLE AS deleted_rows NEW TABLE AS changed_rows FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_eir__refresh_stats_stmt();

CREATE UNIQUE INDEX event_instance_registration_bridge_key ON public.event_instance_registration USING btree (legacy_registration_id, instance_id, COALESCE(person_id, ('-1'::integer)::bigint));
CREATE INDEX event_instance_registration_couple_id_idx ON public.event_instance_registration USING btree (couple_id);
CREATE INDEX event_instance_registration_instance_id_idx ON public.event_instance_registration USING btree (instance_id);
CREATE INDEX event_instance_registration_legacy_registration_id_idx ON public.event_instance_registration USING btree (legacy_registration_id);
CREATE INDEX event_instance_registration_parent_id_idx ON public.event_instance_registration USING btree (parent_registration_id);
CREATE INDEX event_instance_registration_person_id_idx ON public.event_instance_registration USING btree (person_id);
CREATE UNIQUE INDEX event_instance_registration_person_key ON public.event_instance_registration USING btree (instance_id, person_id) WHERE ((person_id IS NOT NULL) AND (legacy_registration_id IS NULL) AND (registration_status = 'active'::public.event_instance_registration_status));
CREATE INDEX event_instance_registration_target_cohort_id_idx ON public.event_instance_registration USING btree (target_cohort_id);
CREATE INDEX event_instance_registration_tenant_id_idx ON public.event_instance_registration USING btree (tenant_id);
CREATE UNIQUE INDEX event_instance_registration_unit_key ON public.event_instance_registration USING btree (instance_id, couple_id, person_id) NULLS NOT DISTINCT WHERE (parent_registration_id IS NULL);
