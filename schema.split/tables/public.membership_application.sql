CREATE TABLE public.membership_application (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    first_name text NOT NULL,
    middle_name text,
    last_name text NOT NULL,
    gender public.gender_type NOT NULL,
    birth_date date,
    nationality text NOT NULL,
    tax_identification_number text,
    national_id_number text,
    csts_id text,
    wdsf_id text,
    prefix_title text,
    suffix_title text,
    bio text,
    email public.citext,
    phone text,
    created_by bigint NOT NULL,
    status public.application_form_status NOT NULL,
    note text DEFAULT ''::text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);

COMMENT ON TABLE public.membership_application IS '@omit create,update,delete
@simpleCollections only';

GRANT ALL ON TABLE public.membership_application TO anonymous;
ALTER TABLE public.membership_application ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.membership_application
    ADD CONSTRAINT membership_application_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.membership_application
    ADD CONSTRAINT membership_application_created_by_fkey FOREIGN KEY (created_by) REFERENCES public.users(u_id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.membership_application
    ADD CONSTRAINT membership_application_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_manage ON public.membership_application TO administrator USING (true);
CREATE POLICY my_tenant ON public.membership_application AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id()));

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.membership_application FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
