CREATE TABLE public.event_lesson_request (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    trainer_id bigint NOT NULL,
    registration_id bigint NOT NULL,
    lesson_count integer NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    CONSTRAINT event_lesson_request_lesson_count_check CHECK ((lesson_count > 0))
);

COMMENT ON TABLE public.event_lesson_request IS '@behavior +list -connection -insert -update -delete -query:resource:list -query:resource:connection -query:resource:single';
COMMENT ON COLUMN public.event_lesson_request.registration_id IS '@hasDefault';

GRANT ALL ON TABLE public.event_lesson_request TO anonymous;
ALTER TABLE public.event_lesson_request ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.event_lesson_request
    ADD CONSTRAINT event_lesson_request_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.event_lesson_request
    ADD CONSTRAINT event_lesson_request_registration_id_trainer_id_key UNIQUE (registration_id, trainer_id);
ALTER TABLE ONLY public.event_lesson_request
    ADD CONSTRAINT event_lesson_request_registration_id_fkey FOREIGN KEY (registration_id) REFERENCES public.event_registration(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_lesson_request
    ADD CONSTRAINT event_lesson_request_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_lesson_request
    ADD CONSTRAINT event_lesson_request_trainer_id_fkey FOREIGN KEY (trainer_id) REFERENCES public.event_trainer(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_all ON public.event_lesson_request TO administrator USING (true);
CREATE POLICY current_tenant ON public.event_lesson_request AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY view_visible_instance ON public.event_lesson_request FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.event_registration r
  WHERE (r.id = event_lesson_request.registration_id))));

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event_lesson_request FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

CREATE INDEX event_lesson_request_registration_id_idx ON public.event_lesson_request USING btree (registration_id);
CREATE INDEX event_lesson_request_tenant_id_idx ON public.event_lesson_request USING btree (tenant_id);
CREATE INDEX event_lesson_request_trainer_id_idx ON public.event_lesson_request USING btree (trainer_id);
