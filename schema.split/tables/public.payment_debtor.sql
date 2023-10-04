CREATE TABLE public.payment_debtor (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    payment_id bigint NOT NULL,
    person_id bigint NOT NULL
);

COMMENT ON TABLE public.payment_debtor IS '@omit create,update,delete
@simpleCollections only';

GRANT ALL ON TABLE public.payment_debtor TO anonymous;
ALTER TABLE public.payment_debtor ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.payment_debtor
    ADD CONSTRAINT payment_debtor_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.payment_debtor
    ADD CONSTRAINT payment_debtor_payment_id_fkey FOREIGN KEY (payment_id) REFERENCES public.payment(id);
ALTER TABLE ONLY public.payment_debtor
    ADD CONSTRAINT payment_debtor_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id);
ALTER TABLE ONLY public.payment_debtor
    ADD CONSTRAINT payment_debtor_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id);

CREATE POLICY admin_manage ON public.payment_debtor TO administrator USING (true);
CREATE POLICY my_tenant ON public.payment_debtor AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id()));

