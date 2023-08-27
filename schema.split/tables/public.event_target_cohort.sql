CREATE TABLE public.event_target_cohort (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    event_id bigint NOT NULL,
    cohort_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);

COMMENT ON TABLE public.event_target_cohort IS '@omit create,update,delete
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
CREATE POLICY view_visible_event ON public.event_target_cohort FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.event
  WHERE (event_target_cohort.event_id = event.id))));

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event_target_cohort FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
CREATE TRIGGER _500_register_members AFTER INSERT ON public.event_target_cohort FOR EACH ROW EXECUTE FUNCTION app_private.tg_event_target_cohort__register_members();
CREATE TRIGGER _500_unregister_members AFTER DELETE ON public.event_target_cohort FOR EACH ROW EXECUTE FUNCTION app_private.tg_event_target_cohort__unregister_members();

CREATE INDEX event_target_cohort_cohort_id_idx ON public.event_target_cohort USING btree (cohort_id);
CREATE INDEX event_target_cohort_event_id_idx ON public.event_target_cohort USING btree (event_id);
CREATE INDEX event_target_cohort_tenant_id_idx ON public.event_target_cohort USING btree (tenant_id);