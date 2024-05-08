CREATE TABLE public.event_target_cohort (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    event_id bigint NOT NULL,
    cohort_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);

COMMENT ON TABLE public.event_target_cohort IS '@omit create,update,delete
@foreignKey (cohort_id) references cohort (id)
@simpleCollections only';

GRANT ALL ON TABLE public.event_target_cohort TO anonymous;
ALTER TABLE public.event_target_cohort ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.event_target_cohort
    ADD CONSTRAINT event_target_cohort_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.event_target_cohort
    ADD CONSTRAINT event_target_cohort_cohort_id_fkey FOREIGN KEY (cohort_id) REFERENCES public.skupiny(s_id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_target_cohort
    ADD CONSTRAINT event_target_cohort_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.event(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_target_cohort
    ADD CONSTRAINT event_target_cohort_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_all ON public.event_target_cohort TO administrator USING (true);
CREATE POLICY my_tenant ON public.event_target_cohort AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id()));
CREATE POLICY trainer_same_tenant ON public.event_target_cohort TO trainer USING (app_private.can_trainer_edit_event(event_id)) WITH CHECK ((tenant_id = ANY (public.my_tenants_array())));
CREATE POLICY view_tenant ON public.event_target_cohort FOR SELECT TO member USING (true);

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event_target_cohort FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
CREATE TRIGGER _500_register_members AFTER INSERT ON public.event_target_cohort FOR EACH ROW EXECUTE FUNCTION app_private.tg_event_target_cohort__register_members();
CREATE TRIGGER _500_unregister_members AFTER DELETE ON public.event_target_cohort FOR EACH ROW EXECUTE FUNCTION app_private.tg_event_target_cohort__unregister_members();

CREATE INDEX event_target_cohort_cohort_id_idx ON public.event_target_cohort USING btree (cohort_id);
CREATE INDEX event_target_cohort_event_id_idx ON public.event_target_cohort USING btree (event_id);
CREATE INDEX event_target_cohort_tenant_id_idx ON public.event_target_cohort USING btree (tenant_id);