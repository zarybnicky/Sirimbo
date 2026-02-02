CREATE TABLE public.event_lesson_demand (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    trainer_id bigint NOT NULL,
    registration_id bigint NOT NULL,
    lesson_count integer NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    event_id bigint NOT NULL,
    CONSTRAINT event_lesson_demand_lesson_count_check CHECK ((lesson_count > 0))
);

COMMENT ON TABLE public.event_lesson_demand IS '@omit create,update,delete
@behavior -query:resource:list -query:resource:connection -query:resource:single
@simpleCollections only';
COMMENT ON COLUMN public.event_lesson_demand.registration_id IS '@hasDefault';
COMMENT ON COLUMN public.event_lesson_demand.event_id IS '@omit';

GRANT ALL ON TABLE public.event_lesson_demand TO anonymous;
ALTER TABLE public.event_lesson_demand ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.event_lesson_demand
    ADD CONSTRAINT eld_unique_registration_trainer_key UNIQUE (registration_id, trainer_id);
ALTER TABLE ONLY public.event_lesson_demand
    ADD CONSTRAINT event_lesson_demand_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.event_lesson_demand
    ADD CONSTRAINT event_lesson_demand_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_lesson_demand
    ADD CONSTRAINT event_lesson_demand_tenant_id_registration_id_event_id_fkey FOREIGN KEY (tenant_id, registration_id, event_id) REFERENCES public.event_registration(tenant_id, id, event_id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_lesson_demand
    ADD CONSTRAINT event_lesson_demand_trainer_id_fkey FOREIGN KEY (trainer_id) REFERENCES public.event_trainer(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_all ON public.event_lesson_demand TO administrator USING (true);
CREATE POLICY view_visible_event ON public.event_lesson_demand FOR SELECT USING ((event_id IN ( SELECT event.id
   FROM public.event)));

CREATE TRIGGER _100_event_id BEFORE INSERT OR UPDATE OF registration_id ON public.event_lesson_demand FOR EACH ROW EXECUTE FUNCTION app_private.tg__set_event_id_from_registration_id();
CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event_lesson_demand FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

CREATE INDEX event_lesson_demand_registration_id_idx ON public.event_lesson_demand USING btree (registration_id);
CREATE INDEX event_lesson_demand_tenant_id_idx ON public.event_lesson_demand USING btree (tenant_id);
CREATE INDEX event_lesson_demand_trainer_id_idx ON public.event_lesson_demand USING btree (trainer_id);
