CREATE TABLE public.event_instance (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    event_id bigint NOT NULL,
    location_id bigint,
    range tstzrange NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);

COMMENT ON TABLE public.event_instance IS '@omit create,update,delete';

GRANT ALL ON TABLE public.event_instance TO anonymous;
ALTER TABLE public.event_instance ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.event_instance
    ADD CONSTRAINT event_instance_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.event_instance
    ADD CONSTRAINT event_instance_event_id_fkey FOREIGN KEY (event_id) REFERENCES public.event(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_instance
    ADD CONSTRAINT event_instance_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.location(id) ON UPDATE CASCADE ON DELETE SET NULL;
ALTER TABLE ONLY public.event_instance
    ADD CONSTRAINT event_instance_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event_instance FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

CREATE INDEX event_instance_event_id_idx ON public.event_instance USING btree (event_id);
CREATE INDEX event_instance_location_id_idx ON public.event_instance USING btree (location_id);
CREATE INDEX event_instance_range_idx ON public.event_instance USING gist (range);
CREATE INDEX event_instance_tenant_id_idx ON public.event_instance USING btree (tenant_id);