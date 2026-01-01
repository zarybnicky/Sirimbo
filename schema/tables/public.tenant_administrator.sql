CREATE TABLE public.tenant_administrator (
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    person_id bigint NOT NULL,
    since timestamp with time zone DEFAULT now() NOT NULL,
    until timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    id bigint NOT NULL,
    is_visible boolean DEFAULT true NOT NULL,
    description text DEFAULT ''::text NOT NULL,
    active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[)'::text)) STORED NOT NULL,
    status public.relationship_status DEFAULT 'active'::public.relationship_status NOT NULL,
    CONSTRAINT tenant_administrator_until_gt_since CHECK ((until > since))
);

COMMENT ON TABLE public.tenant_administrator IS '@simpleCollections only';
COMMENT ON COLUMN public.tenant_administrator.active_range IS '@omit';

GRANT ALL ON TABLE public.tenant_administrator TO anonymous;
ALTER TABLE public.tenant_administrator ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.tenant_administrator
    ADD CONSTRAINT tenant_administrator_no_overlap EXCLUDE USING gist (tenant_id WITH =, person_id WITH =, active_range WITH &&);
ALTER TABLE ONLY public.tenant_administrator
    ADD CONSTRAINT tenant_administrator_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.tenant_administrator
    ADD CONSTRAINT tenant_administrator_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.tenant_administrator
    ADD CONSTRAINT tenant_administrator_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_all ON public.tenant_administrator TO administrator USING (true);
CREATE POLICY public_view ON public.tenant_administrator FOR SELECT USING (true);

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.tenant_administrator FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
CREATE TRIGGER _200_refresh_auth_details AFTER INSERT OR DELETE OR UPDATE ON public.tenant_administrator FOR EACH ROW EXECUTE FUNCTION app_private.tg_auth_details__refresh();

CREATE INDEX tenant_administrator_active_by_person ON public.tenant_administrator USING btree (person_id) INCLUDE (tenant_id) WHERE (status = 'active'::public.relationship_status);
CREATE INDEX tenant_administrator_active_by_tenant ON public.tenant_administrator USING btree (tenant_id) INCLUDE (person_id) WHERE (status = 'active'::public.relationship_status);
CREATE INDEX tenant_administrator_person_id_idx ON public.tenant_administrator USING btree (person_id);
CREATE INDEX tenant_administrator_tenant_id_idx ON public.tenant_administrator USING btree (tenant_id);
CREATE INDEX tenant_administrator_tenant_person_active_idx ON public.tenant_administrator USING btree (tenant_id, person_id) WHERE (status = 'active'::public.relationship_status);
CREATE INDEX tenant_administrator_tenant_status_person_idx ON public.tenant_administrator USING btree (tenant_id, status, person_id);
