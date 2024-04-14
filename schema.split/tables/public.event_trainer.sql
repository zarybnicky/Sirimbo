CREATE TABLE public.event_trainer (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    event_id bigint NOT NULL,
    person_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    lessons_offered integer DEFAULT 0 NOT NULL,
    lesson_price public.price DEFAULT NULL::public.price_type
);

COMMENT ON TABLE public.event_trainer IS '@omit create,update,delete
@simpleCollections only';

GRANT ALL ON TABLE public.event_trainer TO anonymous;
ALTER TABLE public.event_trainer ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.event_trainer
    ADD CONSTRAINT event_trainer_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.event_trainer
    ADD CONSTRAINT event_trainer_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.event(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_trainer
    ADD CONSTRAINT event_trainer_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_trainer
    ADD CONSTRAINT event_trainer_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_all ON public.event_trainer TO administrator USING (true);
CREATE POLICY my_tenant ON public.event_trainer AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id()));
CREATE POLICY trainer_same_tenant ON public.event_trainer TO trainer USING (app_private.can_trainer_edit_event(event_id)) WITH CHECK ((tenant_id = ANY (public.my_tenants_array())));
CREATE POLICY view_all ON public.event_trainer FOR SELECT TO member USING (true);

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event_trainer FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

CREATE INDEX event_trainer_event_id_idx ON public.event_trainer USING btree (event_id);
CREATE INDEX event_trainer_person_id_idx ON public.event_trainer USING btree (person_id);
CREATE INDEX event_trainer_tenant_id_idx ON public.event_trainer USING btree (tenant_id);