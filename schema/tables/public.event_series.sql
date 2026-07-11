CREATE TABLE public.event_series (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    name text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);

COMMENT ON TABLE public.event_series IS '@behavior -query:resource:list -query:resource:connection -query:resource:single';

GRANT ALL ON TABLE public.event_series TO anonymous;
ALTER TABLE public.event_series ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.event_series
    ADD CONSTRAINT event_series_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.event_series
    ADD CONSTRAINT event_series_tenant_id_id_key UNIQUE (tenant_id, id);
ALTER TABLE ONLY public.event_series
    ADD CONSTRAINT event_series_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_all ON public.event_series TO administrator USING (true) WITH CHECK (true);
CREATE POLICY current_tenant ON public.event_series AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id))) WITH CHECK ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY member_view ON public.event_series FOR SELECT TO member USING ((EXISTS ( SELECT 1
   FROM public.event_instance instance
  WHERE (instance.series_id = event_series.id))));
CREATE POLICY public_view ON public.event_series FOR SELECT TO anonymous USING ((EXISTS ( SELECT 1
   FROM public.event_instance instance
  WHERE (instance.series_id = event_series.id))));
CREATE POLICY trainer_all ON public.event_series TO trainer USING (true) WITH CHECK (true);

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event_series FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
