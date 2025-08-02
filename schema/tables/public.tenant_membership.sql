CREATE TABLE public.tenant_membership (
    tenant_id bigint NOT NULL,
    person_id bigint NOT NULL,
    since timestamp with time zone DEFAULT now() NOT NULL,
    until timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    id bigint NOT NULL,
    active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[]'::text)) STORED NOT NULL,
    status public.relationship_status DEFAULT 'active'::public.relationship_status NOT NULL,
    active boolean GENERATED ALWAYS AS ((status = 'active'::public.relationship_status)) STORED NOT NULL
);

COMMENT ON TABLE public.tenant_membership IS '@simpleCollections only';
COMMENT ON COLUMN public.tenant_membership.active_range IS '@omit';

GRANT ALL ON TABLE public.tenant_membership TO anonymous;
ALTER TABLE public.tenant_membership ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.tenant_membership
    ADD CONSTRAINT tenant_membership_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.tenant_membership
    ADD CONSTRAINT tenant_membership_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.tenant_membership
    ADD CONSTRAINT tenant_membership_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_all ON public.tenant_membership TO administrator USING (true);
CREATE POLICY view_visible_person ON public.tenant_membership FOR SELECT USING (true);

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.tenant_membership FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
CREATE TRIGGER _200_refresh_auth_details AFTER INSERT OR DELETE OR UPDATE ON public.tenant_membership FOR EACH ROW EXECUTE FUNCTION app_private.tg_auth_details__refresh();
CREATE TRIGGER _500_on_status AFTER UPDATE ON public.tenant_membership FOR EACH ROW WHEN ((old.status IS DISTINCT FROM new.status)) EXECUTE FUNCTION app_private.tg_tenant_membership__on_status();

CREATE INDEX tenant_membership_active_idx ON public.tenant_membership USING btree (active);
CREATE INDEX tenant_membership_person_id_idx ON public.tenant_membership USING btree (person_id);
CREATE INDEX tenant_membership_range_idx ON public.tenant_membership USING gist (active_range, tenant_id, person_id);
CREATE INDEX tenant_membership_status_idx ON public.tenant_membership USING btree (status);
CREATE INDEX tenant_membership_tenant_id_idx ON public.tenant_membership USING btree (tenant_id);
