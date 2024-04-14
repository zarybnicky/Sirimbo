CREATE TABLE public.event (
    id bigint NOT NULL,
    name text NOT NULL,
    location_text text NOT NULL,
    description text NOT NULL,
    since date,
    until date,
    capacity integer DEFAULT '0'::bigint NOT NULL,
    files_legacy text DEFAULT ''::text NOT NULL,
    updated_at timestamp with time zone,
    is_locked boolean DEFAULT false NOT NULL,
    is_visible boolean DEFAULT false NOT NULL,
    summary text DEFAULT ''::text NOT NULL,
    is_public boolean DEFAULT false NOT NULL,
    enable_notes boolean DEFAULT false NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    description_member text DEFAULT ''::text NOT NULL,
    title_image_legacy text,
    type public.event_type DEFAULT 'camp'::public.event_type NOT NULL,
    location_id bigint,
    payment_type public.event_payment_type DEFAULT 'none'::public.event_payment_type NOT NULL,
    is_paid_by_tenant boolean DEFAULT true NOT NULL,
    member_price public.price DEFAULT NULL::public.price_type,
    guest_price public.price DEFAULT NULL::public.price_type,
    payment_recipient_id bigint,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP
);

COMMENT ON TABLE public.event IS '@omit create';

GRANT ALL ON TABLE public.event TO anonymous;
ALTER TABLE public.event ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.event
    ADD CONSTRAINT idx_23735_primary PRIMARY KEY (id);
ALTER TABLE ONLY public.event
    ADD CONSTRAINT event_location_id_fkey FOREIGN KEY (location_id) REFERENCES public.tenant_location(id);
ALTER TABLE ONLY public.event
    ADD CONSTRAINT event_payment_recipient_id_fkey FOREIGN KEY (payment_recipient_id) REFERENCES public.account(id);
ALTER TABLE ONLY public.event
    ADD CONSTRAINT event_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;

CREATE POLICY admin_same_tenant ON public.event TO administrator USING ((tenant_id = ANY (public.my_tenants_array())));
CREATE POLICY my_tenant ON public.event AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id()));
CREATE POLICY trainer_same_tenant ON public.event TO trainer USING (app_private.can_trainer_edit_event(id)) WITH CHECK ((tenant_id = ANY (public.my_tenants_array())));
CREATE POLICY view_public ON public.event FOR SELECT TO anonymous USING (((is_public = true) OR (tenant_id = ANY (public.my_tenants_array()))));

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.event FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

CREATE INDEX event_type_idx ON public.event USING btree (type);
CREATE INDEX idx_e_tenant ON public.event USING btree (tenant_id);
CREATE INDEX is_visible ON public.event USING btree (is_visible);
CREATE INDEX since ON public.event USING btree (since);