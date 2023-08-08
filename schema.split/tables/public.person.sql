CREATE TABLE public.person (
    id bigint NOT NULL,
    first_name text NOT NULL,
    middle_name text,
    last_name text NOT NULL,
    gender public.gender_type NOT NULL,
    birth_date date NOT NULL,
    nationality text NOT NULL,
    tax_identification_number text,
    national_id_number text,
    csts_id text,
    wdsf_id text,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    legacy_user_id bigint
);

COMMENT ON TABLE public.person IS '@omit create,update,delete';

GRANT ALL ON TABLE public.person TO anonymous;
ALTER TABLE public.person ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.person
    ADD CONSTRAINT person_pkey PRIMARY KEY (id);

CREATE POLICY admin_all ON public.person TO administrator USING (true);
CREATE POLICY view_same_tenant ON public.person FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.tenant_membership
  WHERE ((tenant_membership.active = true) AND (tenant_membership.person_id = tenant_membership.id) AND (tenant_membership.tenant_id IN ( SELECT public.my_tenant_ids() AS my_tenant_ids))))));
CREATE POLICY view_tenant_admin ON public.person FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.tenant_administrator
  WHERE ((tenant_administrator.active = true) AND (tenant_administrator.person_id = tenant_administrator.id)))));
CREATE POLICY view_tenant_trainer ON public.person FOR SELECT USING ((EXISTS ( SELECT 1
   FROM public.tenant_trainer
  WHERE ((tenant_trainer.active = true) AND (tenant_trainer.person_id = tenant_trainer.id)))));

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.person FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
