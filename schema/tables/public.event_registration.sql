CREATE TABLE public.event_registration (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    event_id bigint NOT NULL,
    parent_registration_id bigint,
    couple_id bigint,
    person_id bigint,
    target_cohort_id bigint,
    status public.attendance_type,
    note text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    attendance_created_at timestamp with time zone,
    attendance_updated_at timestamp with time zone,
    attendance_note text,
    registration_status public.event_registration_status DEFAULT 'active'::public.event_registration_status NOT NULL,
    source public.event_registration_source,
    created_by bigint DEFAULT public.current_user_id(),
    CONSTRAINT event_registration_attendance_state CHECK (((person_id IS NULL) OR ((status IS NOT NULL) AND (attendance_created_at IS NOT NULL) AND (attendance_updated_at IS NOT NULL)))),
    CONSTRAINT event_registration_shape CHECK (
CASE
    WHEN (parent_registration_id IS NULL) THEN (((couple_id IS NOT NULL) AND (person_id IS NULL)) OR ((couple_id IS NULL) AND (person_id IS NOT NULL)))
    ELSE ((person_id IS NOT NULL) AND (couple_id IS NULL))
END)
);

COMMENT ON TABLE public.event_registration IS '@behavior +list +connection -insert -update -delete';
COMMENT ON COLUMN public.event_registration.created_by IS '@behavior -*';

GRANT ALL ON TABLE public.event_registration TO anonymous;
ALTER TABLE public.event_registration ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_couple_id_fkey FOREIGN KEY (couple_id) REFERENCES public.couple(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_created_by_fkey FOREIGN KEY (created_by) REFERENCES public.users(id) ON DELETE SET NULL;
ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.event(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_parent_registration_id_fkey FOREIGN KEY (parent_registration_id) REFERENCES public.event_registration(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_target_cohort_id_fkey FOREIGN KEY (target_cohort_id) REFERENCES public.cohort(id) ON UPDATE CASCADE ON DELETE RESTRICT;
ALTER TABLE ONLY public.event_registration
    ADD CONSTRAINT event_registration_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_all ON public.event_registration TO administrator USING (true);
CREATE POLICY current_tenant ON public.event_registration AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY delete_my ON public.event_registration FOR DELETE USING (((person_id = ANY (( SELECT public.current_person_ids() AS current_person_ids)::bigint[])) OR (couple_id = ANY (( SELECT public.current_couple_ids() AS current_couple_ids)::bigint[]))));
CREATE POLICY insert_my ON public.event_registration FOR INSERT WITH CHECK (((person_id = ANY (( SELECT public.current_person_ids() AS current_person_ids)::bigint[])) OR (couple_id = ANY (( SELECT public.current_couple_ids() AS current_couple_ids)::bigint[]))));
CREATE POLICY trainer_insert ON public.event_registration FOR INSERT TO trainer WITH CHECK (app_private.can_trainer_edit_event(event_id));
CREATE POLICY trainer_update ON public.event_registration FOR UPDATE TO trainer USING (app_private.can_trainer_edit_event(event_id));
CREATE POLICY view_visible_instance ON public.event_registration FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.event e
  WHERE (e.id = event_registration.event_id))));

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event_registration FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps('attendance_created_at', 'attendance_updated_at');
CREATE TRIGGER _200_attendance_timestamps BEFORE INSERT OR UPDATE OF person_id, status, attendance_note ON public.event_registration FOR EACH ROW EXECUTE FUNCTION app_private.tg_event_registration__attendance_timestamps();
CREATE TRIGGER _500_refresh_stats_del AFTER DELETE ON public.event_registration REFERENCING OLD TABLE AS deleted_rows FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_event_registration__refresh_stats_stmt();
CREATE TRIGGER _500_refresh_stats_ins AFTER INSERT ON public.event_registration REFERENCING NEW TABLE AS changed_rows FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_event_registration__refresh_stats_stmt();
CREATE TRIGGER _500_refresh_stats_upd AFTER UPDATE ON public.event_registration REFERENCING OLD TABLE AS deleted_rows NEW TABLE AS changed_rows FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_event_registration__refresh_stats_stmt();
CREATE CONSTRAINT TRIGGER _800_sync_lesson_payment AFTER INSERT OR DELETE OR UPDATE OF event_id, person_id, registration_status ON public.event_registration DEFERRABLE INITIALLY DEFERRED FOR EACH ROW EXECUTE FUNCTION app_private.tg_event_registration__sync_lesson_payment();
CREATE TRIGGER _900_refresh_event_people AFTER INSERT OR DELETE OR UPDATE OF tenant_id, event_id, person_id, registration_status ON public.event_registration FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_event_people__refresh();

CREATE INDEX event_registration_active_person_idx ON public.event_registration USING btree (tenant_id, person_id) INCLUDE (event_id, status) WHERE (registration_status = 'active'::public.event_registration_status);
CREATE INDEX event_registration_couple_id_idx ON public.event_registration USING btree (couple_id);
CREATE INDEX event_registration_event_id_id_idx ON public.event_registration USING btree (event_id, id);
CREATE INDEX event_registration_parent_id_idx ON public.event_registration USING btree (parent_registration_id);
CREATE INDEX event_registration_person_id_idx ON public.event_registration USING btree (person_id);
CREATE UNIQUE INDEX event_registration_person_key ON public.event_registration USING btree (event_id, person_id) WHERE ((person_id IS NOT NULL) AND (registration_status = 'active'::public.event_registration_status));
CREATE INDEX event_registration_target_cohort_id_idx ON public.event_registration USING btree (target_cohort_id);
CREATE INDEX event_registration_tenant_id_idx ON public.event_registration USING btree (tenant_id);
CREATE UNIQUE INDEX event_registration_unit_key ON public.event_registration USING btree (event_id, couple_id, person_id) NULLS NOT DISTINCT WHERE (parent_registration_id IS NULL);
