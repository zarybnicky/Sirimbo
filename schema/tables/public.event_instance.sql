CREATE TABLE public.event_instance (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    event_id bigint,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    since timestamp with time zone NOT NULL,
    until timestamp with time zone NOT NULL,
    is_cancelled boolean DEFAULT false NOT NULL,
    range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[)'::text)) STORED NOT NULL,
    name text,
    type public.event_type,
    location_text text,
    location_id bigint,
    is_visible boolean,
    is_public boolean,
    custom jsonb DEFAULT '{}'::jsonb NOT NULL,
    manager_person_ids bigint[] DEFAULT '{}'::bigint[] NOT NULL,
    stats jsonb DEFAULT '{}'::jsonb NOT NULL,
    parent_id bigint,
    capacity integer,
    capacity_unit public.event_capacity_unit DEFAULT 'registrations'::public.event_capacity_unit,
    description text,
    summary text,
    is_locked boolean,
    enable_notes boolean,
    files_legacy text,
    CONSTRAINT event_instance_until_gt_since CHECK ((until > since))
);

COMMENT ON TABLE public.event_instance IS '@omit create,delete
@simpleCollections only
@behavior -query:resource:list -query:resource:connection';

GRANT ALL ON TABLE public.event_instance TO anonymous;
ALTER TABLE public.event_instance ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.event_instance
    ADD CONSTRAINT event_instance_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.event_instance
    ADD CONSTRAINT event_instance_tenant_id_id_event_id_ux UNIQUE (tenant_id, id, event_id);
ALTER TABLE ONLY public.event_instance
    ADD CONSTRAINT event_instance_tenant_id_id_key UNIQUE (tenant_id, id);
ALTER TABLE ONLY public.event_instance
    ADD CONSTRAINT event_instance_event_fkey FOREIGN KEY (tenant_id, event_id) REFERENCES public.event(tenant_id, id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_instance
    ADD CONSTRAINT event_instance_location_fkey FOREIGN KEY (tenant_id, location_id) REFERENCES public.tenant_location(tenant_id, id) ON UPDATE CASCADE ON DELETE SET NULL;
ALTER TABLE ONLY public.event_instance
    ADD CONSTRAINT event_instance_parent_id_fkey FOREIGN KEY (parent_id) REFERENCES public.event_instance(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.event_instance
    ADD CONSTRAINT event_instance_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_same_tenant ON public.event_instance TO administrator USING (true);
CREATE POLICY current_tenant ON public.event_instance AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY member_view ON public.event_instance FOR SELECT TO member USING (is_visible);
CREATE POLICY public_view ON public.event_instance FOR SELECT TO anonymous USING (is_public);
CREATE POLICY trainer_same_tenant ON public.event_instance TO trainer USING (app_private.can_trainer_edit_instance(id)) WITH CHECK (true);

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event_instance FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
CREATE TRIGGER _500_create_attendance AFTER INSERT ON public.event_instance FOR EACH ROW EXECUTE FUNCTION app_private.tg_event_instance__create_attendance();
CREATE TRIGGER _500_delete_on_cancellation AFTER UPDATE OF is_cancelled ON public.event_instance FOR EACH ROW EXECUTE FUNCTION app_private.tg_event_instance__delete_payment_on_cancellation();
CREATE TRIGGER _600_reparent_eir_instance AFTER UPDATE OF event_id ON public.event_instance FOR EACH ROW EXECUTE FUNCTION app_private.tg_event_instance__reparent_eir();
CREATE TRIGGER _600_sync_eir_instance_ins AFTER INSERT ON public.event_instance REFERENCING NEW TABLE AS changed_rows FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_event_instance__sync_eir();
CREATE TRIGGER _800_event_instance__fill_defaults BEFORE INSERT ON public.event_instance FOR EACH ROW EXECUTE FUNCTION public.tg_event_instance__fill_defaults();
CREATE TRIGGER _800_event_instance__pin_overrides BEFORE UPDATE OF event_id, name, type, location_text, location_id, is_visible, is_public, capacity, is_locked, description, summary, enable_notes, files_legacy, custom ON public.event_instance FOR EACH ROW EXECUTE FUNCTION public.tg_event_instance__pin_overrides();

CREATE INDEX event_instance_event_id_idx ON public.event_instance USING btree (event_id);
CREATE INDEX event_instance_parent_id_idx ON public.event_instance USING btree (parent_id);
CREATE INDEX event_instance_range_idx ON public.event_instance USING gist (range);
CREATE INDEX event_instance_since_idx ON public.event_instance USING btree (since);
CREATE INDEX event_instance_tenant_idx ON public.event_instance USING btree (tenant_id);
CREATE INDEX event_instance_tenant_range_gist ON public.event_instance USING gist (tenant_id, range);
CREATE INDEX event_instance_tenant_since_idx ON public.event_instance USING btree (tenant_id, since);
CREATE INDEX event_instance_tenant_until_idx ON public.event_instance USING btree (tenant_id, until);
