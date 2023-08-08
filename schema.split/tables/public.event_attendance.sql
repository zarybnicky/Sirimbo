CREATE TABLE public.event_attendance (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    instance_id bigint NOT NULL,
    person_id bigint NOT NULL,
    status public.attendance_type DEFAULT 'unknown'::public.attendance_type NOT NULL,
    note text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);

COMMENT ON TABLE public.event_attendance IS '@omit create,update,delete
@simpleCollections only';

GRANT ALL ON TABLE public.event_attendance TO anonymous;
ALTER TABLE public.event_attendance ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.event_attendance
    ADD CONSTRAINT event_attendance_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.event_attendance
    ADD CONSTRAINT event_attendance_instance_id_fkey FOREIGN KEY (instance_id) REFERENCES public.event_instance(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_attendance
    ADD CONSTRAINT event_attendance_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_attendance
    ADD CONSTRAINT event_attendance_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_all ON public.event_attendance TO administrator USING (true);
CREATE POLICY view_visible_event ON public.event_attendance FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.event_instance
  WHERE (event_attendance.instance_id = event_instance.id))));

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event_attendance FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

CREATE INDEX event_attendance_instance_id_idx ON public.event_attendance USING btree (instance_id);
CREATE INDEX event_attendance_person_id_idx ON public.event_attendance USING btree (person_id);
CREATE INDEX event_attendance_tenant_id_idx ON public.event_attendance USING btree (tenant_id);