CREATE TABLE public.event_trainer (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    event_id bigint NOT NULL,
    person_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    lessons_offered integer DEFAULT 0 NOT NULL
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
CREATE POLICY view_visible_event ON public.event_trainer FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.event
  WHERE (event_trainer.event_id = event.id))));

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event_trainer FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

CREATE INDEX event_trainer_event_id_idx ON public.event_trainer USING btree (event_id);
CREATE INDEX event_trainer_person_id_idx ON public.event_trainer USING btree (person_id);
CREATE INDEX event_trainer_tenant_id_idx ON public.event_trainer USING btree (tenant_id);