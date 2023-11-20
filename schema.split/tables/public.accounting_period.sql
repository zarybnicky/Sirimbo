CREATE TABLE public.accounting_period (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    name text DEFAULT ''::text NOT NULL,
    since timestamp with time zone NOT NULL,
    until timestamp with time zone NOT NULL,
    range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[]'::text)) STORED NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);

COMMENT ON TABLE public.accounting_period IS '@omit create,update,delete
@simpleCollections only';

GRANT ALL ON TABLE public.accounting_period TO anonymous;
ALTER TABLE public.accounting_period ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.accounting_period
    ADD CONSTRAINT accounting_period_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.accounting_period
    ADD CONSTRAINT accounting_period_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_manage ON public.accounting_period TO administrator USING (true);
CREATE POLICY my_tenant ON public.accounting_period AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id()));
CREATE POLICY person_view ON public.accounting_period FOR SELECT TO anonymous USING (true);

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.accounting_period FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
