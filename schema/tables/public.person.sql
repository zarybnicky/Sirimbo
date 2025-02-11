CREATE TABLE public.person (
    id bigint NOT NULL,
    first_name text NOT NULL,
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
    phone text,
    name text GENERATED ALWAYS AS (public.immutable_concat_ws(' '::text, VARIADIC ARRAY[NULLIF(TRIM(BOTH FROM prefix_title), ''::text), NULLIF(TRIM(BOTH FROM first_name), ''::text), NULLIF(TRIM(BOTH FROM last_name), ''::text),
CASE
    WHEN ((suffix_title IS NULL) OR (TRIM(BOTH FROM suffix_title) = ''::text)) THEN NULL::text
    ELSE public.immutable_concat_ws(' '::text, VARIADIC ARRAY[','::text, TRIM(BOTH FROM suffix_title)])
END])) STORED NOT NULL,
    address public.address_domain
);

COMMENT ON TABLE public.person IS '@omit create';
COMMENT ON COLUMN public.person.legacy_user_id IS '@omit';

GRANT ALL ON TABLE public.person TO anonymous;
ALTER TABLE public.person ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.person
    ADD CONSTRAINT person_pkey PRIMARY KEY (id);

CREATE POLICY admin_all ON public.person TO administrator USING (true);
CREATE POLICY admin_myself ON public.person FOR UPDATE USING ((id = ANY (public.current_person_ids())));
CREATE POLICY view_tenant_or_trainer ON public.person FOR SELECT USING (( SELECT ((( SELECT public.current_tenant_id() AS current_tenant_id) = ANY (auth_details.allowed_tenants)) AND ((( SELECT public.current_tenant_id() AS current_tenant_id) IN ( SELECT public.my_tenant_ids() AS my_tenant_ids)) OR (( SELECT public.current_tenant_id() AS current_tenant_id) = ANY (auth_details.tenant_trainers)) OR (( SELECT public.current_tenant_id() AS current_tenant_id) = ANY (auth_details.tenant_administrators))))
   FROM public.auth_details
  WHERE (auth_details.person_id = person.id)));

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.person FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
