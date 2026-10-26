CREATE TABLE public.event (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
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
    manager_person_ids bigint[] DEFAULT '{}'::bigint[] NOT NULL,
    stats jsonb DEFAULT '{}'::jsonb NOT NULL,
    parent_id bigint,
    capacity integer,
    capacity_unit public.event_capacity_unit DEFAULT 'people'::public.event_capacity_unit NOT NULL,
    description text,
    summary text,
    is_locked boolean,
    enable_notes boolean,
    files_legacy text,
    series_id bigint,
    has_public_details boolean DEFAULT false NOT NULL,
    share_token text,
    cover_image_id bigint,
    series_info public.series_info,
    CONSTRAINT event_public_details_require_public CHECK (((NOT has_public_details) OR (is_public IS TRUE))),
    CONSTRAINT event_until_gt_since CHECK ((until > since))
);

COMMENT ON TABLE public.event IS '@behavior +list -connection -insert';
COMMENT ON COLUMN public.event.share_token IS '@behavior -*';
COMMENT ON COLUMN public.event.series_info IS '@behavior -attribute:insert -attribute:update -attribute:filterBy -attribute:orderBy';

GRANT ALL ON TABLE public.event TO anonymous;
ALTER TABLE public.event ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.event
    ADD CONSTRAINT event_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.event
    ADD CONSTRAINT event_tenant_id_id_key UNIQUE (tenant_id, id);
ALTER TABLE ONLY public.event
    ADD CONSTRAINT event_cover_image_fk FOREIGN KEY (tenant_id, cover_image_id) REFERENCES public.file(tenant_id, id) ON DELETE SET NULL (cover_image_id);
ALTER TABLE ONLY public.event
    ADD CONSTRAINT event_location_fkey FOREIGN KEY (tenant_id, location_id) REFERENCES public.location(tenant_id, id) ON UPDATE CASCADE ON DELETE SET NULL;
ALTER TABLE ONLY public.event
    ADD CONSTRAINT event_parent_id_fkey FOREIGN KEY (tenant_id, parent_id) REFERENCES public.event(tenant_id, id) ON UPDATE CASCADE;
ALTER TABLE ONLY public.event
    ADD CONSTRAINT event_series_fkey FOREIGN KEY (tenant_id, series_id) REFERENCES public.event_series(tenant_id, id) ON UPDATE CASCADE ON DELETE SET NULL (series_id);
ALTER TABLE ONLY public.event
    ADD CONSTRAINT event_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_same_tenant ON public.event TO administrator USING (true);
CREATE POLICY current_tenant ON public.event AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY event_access_view ON public.event FOR SELECT TO anonymous USING ((id IN ( SELECT app_private.accessible_event_ids() AS accessible_event_ids)));
CREATE POLICY member_view ON public.event FOR SELECT TO member USING (is_visible);
CREATE POLICY public_view ON public.event FOR SELECT TO anonymous USING (is_public);
CREATE POLICY trainer_delete ON public.event FOR DELETE TO trainer USING (app_private.can_trainer_edit_event(id));
CREATE POLICY trainer_insert ON public.event FOR INSERT TO trainer WITH CHECK (((parent_id IS NULL) OR app_private.can_trainer_edit_event(parent_id)));
CREATE POLICY trainer_select ON public.event FOR SELECT TO trainer USING (((cardinality(manager_person_ids) = 0) OR (manager_person_ids && ( SELECT public.current_person_ids() AS current_person_ids))));
CREATE POLICY trainer_update ON public.event FOR UPDATE TO trainer USING (app_private.can_trainer_edit_event(id));

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps('manager_person_ids', 'stats', 'series_info');
CREATE TRIGGER _200_validate_parent BEFORE INSERT OR UPDATE OF tenant_id, parent_id ON public.event FOR EACH ROW EXECUTE FUNCTION app_private.tg_event__validate_parent();
CREATE TRIGGER _500_delete_on_cancellation AFTER UPDATE OF is_cancelled ON public.event FOR EACH ROW WHEN ((old.is_cancelled IS DISTINCT FROM new.is_cancelled)) EXECUTE FUNCTION app_private.tg_event__delete_payment_on_cancellation();
CREATE TRIGGER _500_refresh_manager_person_ids_from_parent AFTER INSERT OR UPDATE OF parent_id ON public.event FOR EACH ROW EXECUTE FUNCTION app_private.tg_event__refresh_manager_person_ids();
CREATE TRIGGER _600_refresh_series_info_del AFTER DELETE ON public.event REFERENCING OLD TABLE AS deleted_rows FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_event__refresh_series_info();
CREATE TRIGGER _600_refresh_series_info_ins AFTER INSERT ON public.event REFERENCING NEW TABLE AS changed_rows FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_event__refresh_series_info();
CREATE TRIGGER _600_refresh_series_info_upd AFTER UPDATE ON public.event REFERENCING OLD TABLE AS deleted_rows NEW TABLE AS changed_rows FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_event__refresh_series_info();
CREATE TRIGGER _900_refresh_event_people AFTER INSERT OR DELETE OR UPDATE OF tenant_id, parent_id ON public.event FOR EACH STATEMENT EXECUTE FUNCTION app_private.tg_event_people__refresh();

CREATE INDEX event_cover_image_idx ON public.event USING btree (tenant_id, cover_image_id) WHERE (cover_image_id IS NOT NULL);
CREATE INDEX event_parent_id_idx ON public.event USING btree (parent_id);
CREATE INDEX event_range_idx ON public.event USING gist (range);
CREATE UNIQUE INDEX event_share_token_key ON public.event USING btree (share_token) WHERE (share_token IS NOT NULL);
CREATE INDEX event_since_idx ON public.event USING btree (since);
CREATE INDEX event_tenant_idx ON public.event USING btree (tenant_id);
CREATE INDEX event_tenant_range_gist ON public.event USING gist (tenant_id, range);
CREATE INDEX event_tenant_series_idx ON public.event USING btree (tenant_id, series_id);
CREATE INDEX event_tenant_since_idx ON public.event USING btree (tenant_id, since);
CREATE INDEX event_tenant_until_idx ON public.event USING btree (tenant_id, until);
