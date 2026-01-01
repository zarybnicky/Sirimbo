CREATE TABLE public.tenant_trainer (
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    person_id bigint NOT NULL,
    since timestamp with time zone DEFAULT now() NOT NULL,
    until timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    id bigint NOT NULL,
    is_visible boolean DEFAULT true,
    description text DEFAULT ''::text NOT NULL,
    active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[)'::text)) STORED NOT NULL,
    member_price_45min public.price DEFAULT NULL::public.price_type,
    member_payout_45min public.price DEFAULT NULL::public.price_type,
    guest_price_45min public.price DEFAULT NULL::public.price_type,
    guest_payout_45min public.price DEFAULT NULL::public.price_type,
    create_payout_payments boolean DEFAULT true NOT NULL,
    status public.relationship_status DEFAULT 'active'::public.relationship_status NOT NULL,
    CONSTRAINT tenant_trainer_until_gt_since CHECK ((until > since))
);

COMMENT ON TABLE public.tenant_trainer IS '@simpleCollections only';
COMMENT ON COLUMN public.tenant_trainer.active_range IS '@omit';

GRANT ALL ON TABLE public.tenant_trainer TO anonymous;
ALTER TABLE public.tenant_trainer ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.tenant_trainer
    ADD CONSTRAINT tenant_trainer_no_overlap EXCLUDE USING gist (tenant_id WITH =, person_id WITH =, active_range WITH &&);
ALTER TABLE ONLY public.tenant_trainer
    ADD CONSTRAINT tenant_trainer_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.tenant_trainer
    ADD CONSTRAINT tenant_trainer_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.tenant_trainer
    ADD CONSTRAINT tenant_trainer_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_all ON public.tenant_trainer TO administrator USING (true);
CREATE POLICY public_view ON public.tenant_trainer FOR SELECT USING (true);

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.tenant_trainer FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
CREATE TRIGGER _200_refresh_auth_details AFTER INSERT OR DELETE OR UPDATE ON public.tenant_trainer FOR EACH ROW EXECUTE FUNCTION app_private.tg_auth_details__refresh();

CREATE INDEX tenant_trainer_active_by_person ON public.tenant_trainer USING btree (person_id) INCLUDE (tenant_id) WHERE (status = 'active'::public.relationship_status);
CREATE INDEX tenant_trainer_active_by_tenant ON public.tenant_trainer USING btree (tenant_id) INCLUDE (person_id) WHERE (status = 'active'::public.relationship_status);
CREATE INDEX tenant_trainer_person_id_idx ON public.tenant_trainer USING btree (person_id);
CREATE INDEX tenant_trainer_tenant_id_idx ON public.tenant_trainer USING btree (tenant_id);
CREATE INDEX tenant_trainer_tenant_person_active_idx ON public.tenant_trainer USING btree (tenant_id, person_id) WHERE (status = 'active'::public.relationship_status);
CREATE INDEX tenant_trainer_tenant_status_person_idx ON public.tenant_trainer USING btree (tenant_id, status, person_id);
