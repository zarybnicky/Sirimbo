CREATE TABLE public.tenant_administrator (
    tenant_id bigint NOT NULL,
    person_id bigint NOT NULL,
    since timestamp with time zone DEFAULT now() NOT NULL,
    until timestamp with time zone,
    active boolean DEFAULT true NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);

COMMENT ON TABLE public.tenant_administrator IS '@omit create,update,delete';

GRANT ALL ON TABLE public.tenant_administrator TO anonymous;
ALTER TABLE public.tenant_administrator ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.tenant_administrator
    ADD CONSTRAINT tenant_administrator_pkey PRIMARY KEY (tenant_id, person_id);
ALTER TABLE ONLY public.tenant_administrator
    ADD CONSTRAINT tenant_administrator_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.tenant_administrator
    ADD CONSTRAINT tenant_administrator_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.tenant_administrator FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

CREATE INDEX tenant_administrator_active_idx ON public.tenant_administrator USING btree (active);
CREATE INDEX tenant_administrator_person_id_idx ON public.tenant_administrator USING btree (person_id);
CREATE INDEX tenant_administrator_tenant_id_idx ON public.tenant_administrator USING btree (tenant_id);