CREATE TABLE public.users (
    u_id bigint NOT NULL,
    u_login text NOT NULL,
    u_pass character(40) NOT NULL,
    u_jmeno text NOT NULL,
    u_prijmeni text NOT NULL,
    u_email text NOT NULL,
    u_poznamky text DEFAULT ''::text NOT NULL,
    u_timestamp timestamp with time zone DEFAULT now() NOT NULL,
    u_ban boolean DEFAULT true NOT NULL,
    u_confirmed boolean DEFAULT false NOT NULL,
    u_system boolean DEFAULT true NOT NULL,
    u_nationality text NOT NULL,
    u_member_since timestamp with time zone,
    u_member_until timestamp with time zone,
    u_created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    u_gdpr_signed_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    id bigint GENERATED ALWAYS AS (u_id) STORED,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);

COMMENT ON TABLE public.users IS '@omit create,update,delete';
COMMENT ON COLUMN public.users.u_pass IS '@omit';
COMMENT ON COLUMN public.users.u_poznamky IS '@omit';
COMMENT ON COLUMN public.users.u_ban IS '@omit';
COMMENT ON COLUMN public.users.u_confirmed IS '@omit';
COMMENT ON COLUMN public.users.u_system IS '@omit';
COMMENT ON COLUMN public.users.u_member_since IS '@omit';
COMMENT ON COLUMN public.users.u_member_until IS '@omit';
COMMENT ON COLUMN public.users.u_gdpr_signed_at IS '@omit';

GRANT ALL ON TABLE public.users TO anonymous;
GRANT INSERT(u_id) ON TABLE public.users TO anonymous;
GRANT INSERT(u_login) ON TABLE public.users TO anonymous;
GRANT INSERT(u_pass) ON TABLE public.users TO anonymous;
GRANT INSERT(u_jmeno) ON TABLE public.users TO anonymous;
GRANT INSERT(u_prijmeni) ON TABLE public.users TO anonymous;
GRANT INSERT(u_email) ON TABLE public.users TO anonymous;
GRANT INSERT(u_poznamky) ON TABLE public.users TO anonymous;
GRANT INSERT(u_nationality) ON TABLE public.users TO anonymous;
ALTER TABLE public.users ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.users
    ADD CONSTRAINT idx_23964_primary PRIMARY KEY (u_id);
ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);

CREATE POLICY admin_all ON public.users TO administrator USING (true) WITH CHECK (true);
CREATE POLICY all_view ON public.users FOR SELECT TO member USING (true);
CREATE POLICY manage_own ON public.users USING ((u_id = public.current_user_id())) WITH CHECK ((u_id = public.current_user_id()));
CREATE POLICY my_tenant ON public.users AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));
CREATE POLICY register_anonymous ON public.users FOR INSERT WITH CHECK ((u_confirmed = false));

CREATE TRIGGER _200_encrypt_password BEFORE INSERT OR UPDATE ON public.users FOR EACH ROW EXECUTE FUNCTION app_private.tg_users__encrypt_password();
CREATE TRIGGER _500_notify_admin AFTER INSERT ON public.users FOR EACH ROW EXECUTE FUNCTION app_private.tg_users__notify_admin();
CREATE TRIGGER on_update_current_timestamp BEFORE UPDATE ON public.users FOR EACH ROW EXECUTE FUNCTION public.on_update_current_timestamp_users();

CREATE UNIQUE INDEX idx_23964_u_login ON public.users USING btree (u_login);
CREATE INDEX idx_us_tenant ON public.users USING btree (tenant_id);
CREATE INDEX u_ban ON public.users USING btree (u_ban);
CREATE INDEX u_confirmed ON public.users USING btree (u_confirmed);
CREATE INDEX u_jmeno ON public.users USING btree (u_jmeno);
CREATE INDEX u_prijmeni ON public.users USING btree (u_prijmeni);
CREATE INDEX u_system ON public.users USING btree (u_system);