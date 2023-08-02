CREATE TABLE public.event (
    id bigint NOT NULL,
    name text NOT NULL,
    location_text text NOT NULL,
    description text NOT NULL,
    since date NOT NULL,
    until date NOT NULL,
    capacity bigint DEFAULT '0'::bigint NOT NULL,
    files_legacy text DEFAULT ''::text NOT NULL,
    updated_at timestamp with time zone,
    is_locked boolean DEFAULT false NOT NULL,
    is_visible boolean DEFAULT false NOT NULL,
    summary text DEFAULT '[]'::jsonb NOT NULL,
    is_public boolean DEFAULT false NOT NULL,
    enable_notes boolean DEFAULT false NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    description_member text DEFAULT ''::text NOT NULL,
    title_image_legacy text,
    type public.event_type DEFAULT 'camp'::public.event_type NOT NULL
);

GRANT ALL ON TABLE public.event TO anonymous;
ALTER TABLE public.event ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.event
    ADD CONSTRAINT idx_23735_primary PRIMARY KEY (id);

CREATE POLICY manage_all ON public.event TO administrator USING (true) WITH CHECK (true);
CREATE POLICY my_tenant ON public.event AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));
CREATE POLICY select_member ON public.event FOR SELECT TO member USING (true);
CREATE POLICY select_public ON public.event FOR SELECT TO anonymous USING ((is_public = true));

CREATE TRIGGER on_update_event_timestamp BEFORE INSERT OR UPDATE ON public.event FOR EACH ROW EXECUTE FUNCTION public.on_update_event_timestamp();

CREATE INDEX event_type_idx ON public.event USING btree (type);
CREATE INDEX is_visible ON public.event USING btree (is_visible);
CREATE INDEX since ON public.event USING btree (since);