CREATE TABLE public.person (
    id bigint NOT NULL,
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
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    legacy_user_id bigint,
    prefix_title text DEFAULT ''::text NOT NULL,
    suffix_title text DEFAULT ''::text NOT NULL,
    bio text DEFAULT ''::text NOT NULL,
    email public.citext,
    phone text
);

COMMENT ON TABLE public.person IS '@omit create';
COMMENT ON COLUMN public.person.middle_name IS '@deprecated';

GRANT ALL ON TABLE public.person TO anonymous;
ALTER TABLE public.person ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.person
    ADD CONSTRAINT person_pkey PRIMARY KEY (id);

CREATE POLICY admin_all ON public.person TO administrator USING (true);
CREATE POLICY admin_myself ON public.person FOR UPDATE USING ((id IN ( SELECT public.my_person_ids() AS my_person_ids)));
CREATE POLICY view_tenant_or_trainer ON public.person FOR SELECT USING ((true = ( SELECT ((public.current_tenant_id() = ANY (auth_details.allowed_tenants)) AND ((public.current_tenant_id() IN ( SELECT public.my_tenant_ids() AS my_tenant_ids)) OR (public.current_tenant_id() = ANY ((auth_details.tenant_trainers || auth_details.tenant_administrators)))))
   FROM public.auth_details
  WHERE (auth_details.person_id = person.id))));

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.person FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
