CREATE TABLE public.access_credential (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    person_id bigint NOT NULL,
    kind public.access_credential_kind DEFAULT 'MIFARE'::public.access_credential_kind NOT NULL,
    label text NOT NULL,
    code text NOT NULL,
    since timestamp with time zone DEFAULT now() NOT NULL,
    until timestamp with time zone,
    valid_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[)'::text)) STORED,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    created_by bigint DEFAULT public.current_user_id(),
    CONSTRAINT access_credential_code_check CHECK (((code <> ''::text) AND (code = btrim(code)))),
    CONSTRAINT access_credential_label_check CHECK (((label <> ''::text) AND (label = btrim(label)))),
    CONSTRAINT access_credential_until_gt_since CHECK ((until > since))
);

COMMENT ON TABLE public.access_credential IS '@omit delete
@simpleCollections only';
COMMENT ON COLUMN public.access_credential.valid_range IS '@omit';

GRANT ALL ON TABLE public.access_credential TO anonymous;
ALTER TABLE public.access_credential ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.access_credential
    ADD CONSTRAINT access_credential_no_overlap EXCLUDE USING gist (tenant_id WITH =, kind WITH =, code WITH =, valid_range WITH &&);
ALTER TABLE ONLY public.access_credential
    ADD CONSTRAINT access_credential_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.access_credential
    ADD CONSTRAINT access_credential_created_by_fkey FOREIGN KEY (created_by) REFERENCES public.users(id) ON DELETE SET NULL;
ALTER TABLE ONLY public.access_credential
    ADD CONSTRAINT access_credential_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id);
ALTER TABLE ONLY public.access_credential
    ADD CONSTRAINT access_credential_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);

CREATE POLICY admin_insert ON public.access_credential FOR INSERT TO administrator WITH CHECK (((EXISTS ( SELECT 1
   FROM public.tenant_membership r
  WHERE ((r.tenant_id = access_credential.tenant_id) AND (r.person_id = access_credential.person_id)))) OR (EXISTS ( SELECT 1
   FROM public.tenant_trainer r
  WHERE ((r.tenant_id = access_credential.tenant_id) AND (r.person_id = access_credential.person_id)))) OR (EXISTS ( SELECT 1
   FROM public.tenant_administrator r
  WHERE ((r.tenant_id = access_credential.tenant_id) AND (r.person_id = access_credential.person_id))))));
CREATE POLICY admin_update ON public.access_credential FOR UPDATE TO administrator WITH CHECK (((EXISTS ( SELECT 1
   FROM public.tenant_membership r
  WHERE ((r.tenant_id = access_credential.tenant_id) AND (r.person_id = access_credential.person_id)))) OR (EXISTS ( SELECT 1
   FROM public.tenant_trainer r
  WHERE ((r.tenant_id = access_credential.tenant_id) AND (r.person_id = access_credential.person_id)))) OR (EXISTS ( SELECT 1
   FROM public.tenant_administrator r
  WHERE ((r.tenant_id = access_credential.tenant_id) AND (r.person_id = access_credential.person_id))))));
CREATE POLICY admin_view ON public.access_credential FOR SELECT TO administrator USING (true);
CREATE POLICY current_tenant ON public.access_credential AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id()));

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.access_credential FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

CREATE INDEX access_credential_person_idx ON public.access_credential USING btree (tenant_id, person_id);
