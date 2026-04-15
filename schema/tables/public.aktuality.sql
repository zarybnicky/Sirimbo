CREATE TABLE public.aktuality (
    id bigint CONSTRAINT aktuality_at_id_not_null NOT NULL,
    at_kdo bigint,
    at_kat text DEFAULT '1'::text NOT NULL,
    at_jmeno text NOT NULL,
    at_text text NOT NULL,
    at_preview text NOT NULL,
    at_foto bigint,
    updated_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now(),
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    title_photo_url text,
    is_visible boolean DEFAULT true NOT NULL
);

COMMENT ON COLUMN public.aktuality.at_kat IS '@deprecated';

GRANT ALL ON TABLE public.aktuality TO anonymous;
ALTER TABLE public.aktuality ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.aktuality
    ADD CONSTRAINT idx_23753_primary PRIMARY KEY (id);
ALTER TABLE ONLY public.aktuality
    ADD CONSTRAINT aktuality_at_kdo_fkey FOREIGN KEY (at_kdo) REFERENCES public.users(id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.aktuality
    ADD CONSTRAINT aktuality_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON DELETE CASCADE;

CREATE POLICY admin_all ON public.aktuality TO administrator USING (true);
CREATE POLICY current_tenant ON public.aktuality AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY public_view ON public.aktuality FOR SELECT USING (is_visible);

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.aktuality FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
CREATE TRIGGER _200_author BEFORE INSERT OR UPDATE ON public.aktuality FOR EACH ROW EXECUTE FUNCTION app_private.tg_aktuality__author();

CREATE INDEX idx_23753_aktuality_at_kdo_fkey ON public.aktuality USING btree (at_kdo);
CREATE INDEX idx_23753_at_timestamp_add ON public.aktuality USING btree (created_at);
CREATE INDEX tenant_id ON public.aktuality USING btree (tenant_id);
