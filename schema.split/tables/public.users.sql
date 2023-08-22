CREATE TABLE public.users (
    u_id bigint NOT NULL,
    u_login text NOT NULL,
    u_pass character(40) NOT NULL,
    u_jmeno text NOT NULL,
    u_prijmeni text NOT NULL,
    u_pohlavi text NOT NULL,
    u_email text NOT NULL,
    u_telefon text NOT NULL,
    u_narozeni date NOT NULL,
    u_rodne_cislo text,
    u_poznamky text DEFAULT ''::text NOT NULL,
    u_timestamp timestamp with time zone DEFAULT now() NOT NULL,
    u_level smallint DEFAULT '0'::smallint NOT NULL,
    u_group bigint DEFAULT 0 NOT NULL,
    u_skupina bigint DEFAULT '1'::bigint NOT NULL,
    u_dancer boolean DEFAULT true NOT NULL,
    u_ban boolean DEFAULT true NOT NULL,
    u_lock boolean DEFAULT true NOT NULL,
    u_confirmed boolean DEFAULT false NOT NULL,
    u_system boolean DEFAULT true NOT NULL,
    u_street text NOT NULL,
    u_conscription_number text DEFAULT ''::text NOT NULL,
    u_orientation_number text DEFAULT ''::text NOT NULL,
    u_district text DEFAULT ''::text NOT NULL,
    u_city text NOT NULL,
    u_postal_code text NOT NULL,
    u_nationality text NOT NULL,
    u_member_since timestamp with time zone,
    u_member_until timestamp with time zone,
    u_created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    u_teacher boolean DEFAULT false NOT NULL,
    u_gdpr_signed_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    id bigint GENERATED ALWAYS AS (u_id) STORED,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL
);

COMMENT ON TABLE public.users IS '@omit create,update,delete';
COMMENT ON COLUMN public.users.u_pass IS '@omit';
COMMENT ON COLUMN public.users.u_pohlavi IS '@omit';
COMMENT ON COLUMN public.users.u_telefon IS '@omit';
COMMENT ON COLUMN public.users.u_narozeni IS '@omit';
COMMENT ON COLUMN public.users.u_rodne_cislo IS '@omit';
COMMENT ON COLUMN public.users.u_poznamky IS '@omit';
COMMENT ON COLUMN public.users.u_level IS '@omit';
COMMENT ON COLUMN public.users.u_group IS '@omit';
COMMENT ON COLUMN public.users.u_skupina IS '@omit';
COMMENT ON COLUMN public.users.u_dancer IS '@omit';
COMMENT ON COLUMN public.users.u_ban IS '@omit';
COMMENT ON COLUMN public.users.u_lock IS '@omit';
COMMENT ON COLUMN public.users.u_confirmed IS '@omit';
COMMENT ON COLUMN public.users.u_system IS '@omit';
COMMENT ON COLUMN public.users.u_member_since IS '@omit';
COMMENT ON COLUMN public.users.u_member_until IS '@omit';
COMMENT ON COLUMN public.users.u_teacher IS '@omit';
COMMENT ON COLUMN public.users.u_gdpr_signed_at IS '@omit';

GRANT ALL ON TABLE public.users TO anonymous;
GRANT INSERT(u_id) ON TABLE public.users TO anonymous;
GRANT INSERT(u_login) ON TABLE public.users TO anonymous;
GRANT INSERT(u_pass) ON TABLE public.users TO anonymous;
GRANT INSERT(u_jmeno) ON TABLE public.users TO anonymous;
GRANT INSERT(u_prijmeni) ON TABLE public.users TO anonymous;
GRANT INSERT(u_pohlavi) ON TABLE public.users TO anonymous;
GRANT INSERT(u_email) ON TABLE public.users TO anonymous;
GRANT INSERT(u_telefon) ON TABLE public.users TO anonymous;
GRANT INSERT(u_narozeni) ON TABLE public.users TO anonymous;
GRANT INSERT(u_rodne_cislo) ON TABLE public.users TO anonymous;
GRANT INSERT(u_poznamky) ON TABLE public.users TO anonymous;
GRANT INSERT(u_dancer) ON TABLE public.users TO anonymous;
GRANT INSERT(u_street) ON TABLE public.users TO anonymous;
GRANT INSERT(u_conscription_number) ON TABLE public.users TO anonymous;
GRANT INSERT(u_orientation_number) ON TABLE public.users TO anonymous;
GRANT INSERT(u_district) ON TABLE public.users TO anonymous;
GRANT INSERT(u_city) ON TABLE public.users TO anonymous;
GRANT INSERT(u_postal_code) ON TABLE public.users TO anonymous;
GRANT INSERT(u_nationality) ON TABLE public.users TO anonymous;
ALTER TABLE public.users ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.users
    ADD CONSTRAINT idx_23964_primary PRIMARY KEY (u_id);
ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_u_group_fkey FOREIGN KEY (u_group) REFERENCES public.permissions(pe_id) ON UPDATE RESTRICT ON DELETE RESTRICT;
ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_u_skupina_fkey FOREIGN KEY (u_skupina) REFERENCES public.skupiny(s_id) ON UPDATE RESTRICT ON DELETE RESTRICT;

CREATE POLICY admin_all ON public.users TO administrator USING (true) WITH CHECK (true);
CREATE POLICY all_view ON public.users FOR SELECT TO member USING (true);
CREATE POLICY manage_own ON public.users USING ((u_id = public.current_user_id())) WITH CHECK ((u_id = public.current_user_id()));
CREATE POLICY my_tenant ON public.users AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id())) WITH CHECK ((tenant_id = public.current_tenant_id()));
CREATE POLICY register_anonymous ON public.users FOR INSERT WITH CHECK ((u_confirmed = false));

CREATE TRIGGER _200_encrypt_password BEFORE INSERT OR UPDATE ON public.users FOR EACH ROW EXECUTE FUNCTION app_private.tg_users__encrypt_password();
CREATE TRIGGER _500_notify_admin AFTER INSERT ON public.users FOR EACH ROW EXECUTE FUNCTION app_private.tg_users__notify_admin();
CREATE TRIGGER on_update_current_timestamp BEFORE UPDATE ON public.users FOR EACH ROW EXECUTE FUNCTION public.on_update_current_timestamp_users();

CREATE UNIQUE INDEX idx_23964_u_login ON public.users USING btree (u_login);
CREATE INDEX idx_23964_u_narozeni ON public.users USING btree (u_narozeni);
CREATE INDEX idx_23964_users_u_group_fkey ON public.users USING btree (u_group);
CREATE INDEX idx_23964_users_u_skupina_fkey ON public.users USING btree (u_skupina);
CREATE INDEX u_ban ON public.users USING btree (u_ban);
CREATE INDEX u_confirmed ON public.users USING btree (u_confirmed);
CREATE INDEX u_jmeno ON public.users USING btree (u_jmeno);
CREATE INDEX u_prijmeni ON public.users USING btree (u_prijmeni);
CREATE INDEX u_system ON public.users USING btree (u_system);