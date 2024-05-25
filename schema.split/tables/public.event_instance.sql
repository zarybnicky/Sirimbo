CREATE TABLE public.event_instance (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    event_id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    since timestamp with time zone NOT NULL,
    until timestamp with time zone NOT NULL,
    range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[]'::text)) STORED NOT NULL,
    location_id bigint,
    is_cancelled boolean DEFAULT false
);

COMMENT ON TABLE public.event_instance IS '@omit create,delete
@simpleCollections only';

GRANT ALL ON TABLE public.event_instance TO anonymous;
ALTER TABLE public.event_instance ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.event_instance
    ADD CONSTRAINT event_instance_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.event_instance
    ADD CONSTRAINT event_instance_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.event(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_instance
    ADD CONSTRAINT event_instance_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.tenant_location(id);
ALTER TABLE ONLY public.event_instance
    ADD CONSTRAINT event_instance_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_same_tenant ON public.event_instance TO administrator USING ((tenant_id IN ( SELECT public.my_tenant_ids() AS my_tenant_ids)));
CREATE POLICY current_tenant ON public.event_instance AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY trainer_same_tenant ON public.event_instance TO trainer USING (app_private.can_trainer_edit_event(event_id)) WITH CHECK ((tenant_id IN ( SELECT public.my_tenant_ids() AS my_tenant_ids)));
CREATE POLICY view_visible_event ON public.event_instance FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.event
  WHERE (event_instance.event_id = event.id))));

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event_instance FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
CREATE TRIGGER _500_create_attendance AFTER INSERT ON public.event_instance FOR EACH ROW EXECUTE FUNCTION app_private.tg_event_instance__create_attendance();

CREATE INDEX event_instance_event_id_idx ON public.event_instance USING btree (event_id);
CREATE INDEX event_instance_range_idx ON public.event_instance USING gist (range);
CREATE INDEX event_instance_since_idx ON public.event_instance USING btree (since);
CREATE INDEX event_instance_tenant_id_idx ON public.event_instance USING btree (tenant_id);