CREATE TABLE public.tenant_membership (
    tenant_id bigint NOT NULL,
    person_id bigint NOT NULL,
    since timestamp with time zone DEFAULT now() NOT NULL,
    until timestamp with time zone,
    active boolean DEFAULT true NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);

COMMENT ON TABLE public.tenant_membership IS '@omit create,update,delete';

GRANT ALL ON TABLE public.tenant_membership TO anonymous;
ALTER TABLE public.tenant_membership ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.tenant_membership
    ADD CONSTRAINT tenant_membership_pkey PRIMARY KEY (tenant_id, person_id);
ALTER TABLE ONLY public.tenant_membership
    ADD CONSTRAINT tenant_membership_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.tenant_membership
    ADD CONSTRAINT tenant_membership_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.tenant_membership FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

CREATE INDEX tenant_membership_person_id_idx ON public.tenant_membership USING btree (person_id);
CREATE INDEX tenant_membership_tenant_id_idx ON public.tenant_membership USING btree (tenant_id);