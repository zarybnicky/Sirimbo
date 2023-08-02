CREATE TABLE public.tenant_trainer (
    tenant_id bigint NOT NULL,
    person_id bigint NOT NULL,
    since timestamp with time zone DEFAULT now() NOT NULL,
    until timestamp with time zone,
    active boolean DEFAULT true NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);

COMMENT ON TABLE public.tenant_trainer IS '@omit create,update,delete';

GRANT ALL ON TABLE public.tenant_trainer TO anonymous;
ALTER TABLE public.tenant_trainer ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.tenant_trainer
    ADD CONSTRAINT tenant_trainer_pkey PRIMARY KEY (tenant_id, person_id);
ALTER TABLE ONLY public.tenant_trainer
    ADD CONSTRAINT tenant_trainer_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.tenant_trainer
    ADD CONSTRAINT tenant_trainer_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.tenant_trainer FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();

CREATE INDEX tenant_trainer_active_idx ON public.tenant_trainer USING btree (active);
CREATE INDEX tenant_trainer_person_id_idx ON public.tenant_trainer USING btree (person_id);
CREATE INDEX tenant_trainer_tenant_id_idx ON public.tenant_trainer USING btree (tenant_id);