CREATE TABLE public.upozorneni (
    up_id bigint NOT NULL,
    up_kdo bigint,
    up_nadpis text NOT NULL,
    up_text text NOT NULL,
    up_barvy bigint DEFAULT '0'::bigint NOT NULL,
    up_lock boolean DEFAULT false NOT NULL,
    updated_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    scheduled_since timestamp with time zone,
    scheduled_until timestamp with time zone,
    is_visible boolean DEFAULT true,
    id bigint GENERATED ALWAYS AS (up_id) STORED NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    sticky boolean DEFAULT false NOT NULL,
    up_timestamp timestamp with time zone GENERATED ALWAYS AS (updated_at) STORED NOT NULL,
    up_timestamp_add timestamp with time zone GENERATED ALWAYS AS (created_at) STORED NOT NULL
);

COMMENT ON COLUMN public.upozorneni.up_barvy IS '@deprecated';

GRANT ALL ON TABLE public.upozorneni TO anonymous;
ALTER TABLE public.upozorneni ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.upozorneni
    ADD CONSTRAINT idx_23943_primary PRIMARY KEY (up_id);
ALTER TABLE ONLY public.upozorneni
    ADD CONSTRAINT upozorneni_unique_id UNIQUE (id);
ALTER TABLE ONLY public.upozorneni
    ADD CONSTRAINT upozorneni_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;
ALTER TABLE ONLY public.upozorneni
    ADD CONSTRAINT upozorneni_up_kdo_fkey FOREIGN KEY (up_kdo) REFERENCES public.users(u_id) ON UPDATE RESTRICT ON DELETE RESTRICT;

CREATE POLICY admin_all ON public.upozorneni TO administrator USING (true);
CREATE POLICY current_tenant ON public.upozorneni AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY member_view ON public.upozorneni FOR SELECT TO member USING (true);

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.upozorneni FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
CREATE TRIGGER on_update_author_upozorneni BEFORE INSERT OR UPDATE ON public.upozorneni FOR EACH ROW EXECUTE FUNCTION public.on_update_author_upozorneni();

CREATE INDEX idx_23943_up_kdo ON public.upozorneni USING btree (up_kdo);
CREATE INDEX idx_23943_up_timestamp_add ON public.upozorneni USING btree (created_at);
CREATE INDEX idx_up_tenant ON public.upozorneni USING btree (tenant_id);