CREATE TABLE public.event_instance_target_cohort (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    instance_id bigint NOT NULL,
    cohort_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);

COMMENT ON TABLE public.event_instance_target_cohort IS '@omit create,update,delete
@behavior -query:resource:list -query:resource:connection -query:resource:single
@simpleCollections only';

GRANT ALL ON TABLE public.event_instance_target_cohort TO anonymous;
ALTER TABLE public.event_instance_target_cohort ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.event_instance_target_cohort
    ADD CONSTRAINT event_instance_target_cohort_instance_id_cohort_id_key UNIQUE (instance_id, cohort_id);
ALTER TABLE ONLY public.event_instance_target_cohort
    ADD CONSTRAINT event_instance_target_cohort_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.event_instance_target_cohort
    ADD CONSTRAINT event_instance_target_cohort_cohort_id_fkey FOREIGN KEY (cohort_id) REFERENCES public.cohort(id) ON UPDATE CASCADE ON DELETE RESTRICT;
ALTER TABLE ONLY public.event_instance_target_cohort
    ADD CONSTRAINT event_instance_target_cohort_instance_id_fkey FOREIGN KEY (instance_id) REFERENCES public.event_instance(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_instance_target_cohort
    ADD CONSTRAINT event_instance_target_cohort_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_all ON public.event_instance_target_cohort TO administrator USING (true);
CREATE POLICY current_tenant ON public.event_instance_target_cohort AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY member_view ON public.event_instance_target_cohort FOR SELECT TO member USING ((instance_id IN ( SELECT event_instance.id
   FROM public.event_instance)));
CREATE POLICY trainer_delete ON public.event_instance_target_cohort FOR DELETE TO trainer USING (app_private.can_trainer_edit_instance(instance_id));
CREATE POLICY trainer_insert ON public.event_instance_target_cohort FOR INSERT TO trainer WITH CHECK (app_private.can_trainer_edit_instance(instance_id));
CREATE POLICY trainer_select ON public.event_instance_target_cohort FOR SELECT TO trainer USING (app_private.can_trainer_edit_instance(instance_id));
CREATE POLICY trainer_update ON public.event_instance_target_cohort FOR UPDATE TO trainer USING (app_private.can_trainer_edit_instance(instance_id)) WITH CHECK (app_private.can_trainer_edit_instance(instance_id));

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event_instance_target_cohort FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
CREATE TRIGGER _500_reconcile_registrations AFTER INSERT OR DELETE OR UPDATE OF instance_id, cohort_id ON public.event_instance_target_cohort FOR EACH ROW EXECUTE FUNCTION app_private.tg_event_instance_target_cohort__reconcile();

CREATE INDEX event_instance_target_cohort_cohort_id_idx ON public.event_instance_target_cohort USING btree (cohort_id);
CREATE INDEX event_instance_target_cohort_tenant_id_idx ON public.event_instance_target_cohort USING btree (tenant_id);
