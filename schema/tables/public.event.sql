CREATE TABLE public.event (
    id bigint CONSTRAINT akce_a_id_not_null NOT NULL,
    name text CONSTRAINT akce_a_jmeno_not_null NOT NULL,
    location_text text CONSTRAINT akce_a_kde_not_null NOT NULL,
    description text CONSTRAINT akce_a_info_not_null NOT NULL,
    capacity integer DEFAULT 0 CONSTRAINT akce_a_kapacita_not_null NOT NULL,
    files_legacy text DEFAULT ''::text CONSTRAINT akce_a_dokumenty_not_null NOT NULL,
    updated_at timestamp with time zone,
    is_locked boolean DEFAULT false CONSTRAINT akce_a_lock_not_null NOT NULL,
    is_visible boolean DEFAULT false CONSTRAINT akce_a_visible_not_null NOT NULL,
    summary text DEFAULT ''::text CONSTRAINT akce_summary_not_null NOT NULL,
    is_public boolean DEFAULT false CONSTRAINT akce_is_public_not_null NOT NULL,
    enable_notes boolean DEFAULT false CONSTRAINT akce_enable_notes_not_null NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    type public.event_type DEFAULT 'camp'::public.event_type NOT NULL,
    location_id bigint,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE public.event IS '@omit create';

GRANT ALL ON TABLE public.event TO anonymous;
ALTER TABLE public.event ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.event
    ADD CONSTRAINT event_tenant_id_id_key UNIQUE (tenant_id, id);
ALTER TABLE ONLY public.event
    ADD CONSTRAINT idx_23735_primary PRIMARY KEY (id);
ALTER TABLE ONLY public.event
    ADD CONSTRAINT event_location_id_fkey FOREIGN KEY (tenant_id, location_id) REFERENCES public.tenant_location(tenant_id, id) ON UPDATE CASCADE ON DELETE SET NULL;
ALTER TABLE ONLY public.event
    ADD CONSTRAINT event_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;

CREATE POLICY admin_same_tenant ON public.event TO administrator USING (true);
CREATE POLICY current_tenant ON public.event AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY member_view ON public.event FOR SELECT TO member USING (is_visible);
CREATE POLICY public_view ON public.event FOR SELECT TO anonymous USING (is_public);
CREATE POLICY trainer_same_tenant ON public.event TO trainer USING (app_private.can_trainer_edit_event(id)) WITH CHECK (true);

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
CREATE TRIGGER _800_event__propagate_to_instances AFTER UPDATE ON public.event FOR EACH ROW EXECUTE FUNCTION public.tg_event__propagate_to_instances();

CREATE INDEX event_location_id_idx ON public.event USING btree (location_id);
CREATE INDEX event_tenant_visible_public ON public.event USING btree (tenant_id) WHERE (is_visible OR is_public);
CREATE INDEX event_tenant_visible_public_idx ON public.event USING btree (tenant_id, is_public, is_visible);
CREATE INDEX event_type_idx ON public.event USING btree (type);
CREATE INDEX event_visible_public_tenant_idx ON public.event USING btree (is_public, is_visible, tenant_id);
CREATE INDEX idx_e_tenant ON public.event USING btree (tenant_id);
CREATE INDEX idx_event_tenant ON public.event USING btree (tenant_id, is_visible);
CREATE INDEX is_visible ON public.event USING btree (is_visible);
