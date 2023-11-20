CREATE TABLE public.transaction (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    accounting_period_id bigint NOT NULL,
    payment_id bigint,
    source public.transaction_source NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    description text
);

COMMENT ON TABLE public.transaction IS '@omit create,update,delete';

GRANT ALL ON TABLE public.transaction TO anonymous;
ALTER TABLE public.transaction ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.transaction
    ADD CONSTRAINT transaction_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.transaction
    ADD CONSTRAINT transaction_accounting_period_id_fkey FOREIGN KEY (accounting_period_id) REFERENCES public.accounting_period(id);
ALTER TABLE ONLY public.transaction
    ADD CONSTRAINT transaction_payment_id_fkey FOREIGN KEY (payment_id) REFERENCES public.payment(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.transaction
    ADD CONSTRAINT transaction_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_manage ON public.transaction TO administrator USING (true);
CREATE POLICY my_tenant ON public.transaction AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id()));
CREATE POLICY person_view ON public.transaction FOR SELECT TO anonymous USING (true);

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.transaction FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
CREATE TRIGGER _200_fill_accounting_period BEFORE INSERT ON public.transaction FOR EACH ROW EXECUTE FUNCTION app_private.tg_payment__fill_accounting_period();
