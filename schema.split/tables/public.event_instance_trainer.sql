CREATE TABLE public.event_instance_trainer (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    instance_id bigint NOT NULL,
    person_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    lesson_price public.price DEFAULT NULL::public.price_type
);

COMMENT ON TABLE public.event_instance_trainer IS '@omit create,update,delete
@simpleCollections only';

GRANT ALL ON TABLE public.event_instance_trainer TO anonymous;
ALTER TABLE public.event_instance_trainer ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.event_instance_trainer
    ADD CONSTRAINT event_instance_trainer_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.event_instance_trainer
    ADD CONSTRAINT event_instance_trainer_instance_id_fkey FOREIGN KEY (instance_id) REFERENCES public.event_instance(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_instance_trainer
    ADD CONSTRAINT event_instance_trainer_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_instance_trainer
    ADD CONSTRAINT event_instance_trainer_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_all ON public.event_instance_trainer TO administrator USING (true);
CREATE POLICY current_tenant ON public.event_instance_trainer AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY member_view ON public.event_instance_trainer FOR SELECT TO member USING (true);
CREATE POLICY trainer_same_tenant ON public.event_instance_trainer TO trainer USING (app_private.can_trainer_edit_event(( SELECT i.event_id
   FROM public.event_instance i
  WHERE (i.id = event_instance_trainer.instance_id)))) WITH CHECK (true);

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event_instance_trainer FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

CREATE INDEX event_instance_trainer_instance_id_idx ON public.event_instance_trainer USING btree (instance_id);
CREATE INDEX event_instance_trainer_person_id_idx ON public.event_instance_trainer USING btree (person_id);
CREATE INDEX event_instance_trainer_tenant_id_idx ON public.event_instance_trainer USING btree (tenant_id);
