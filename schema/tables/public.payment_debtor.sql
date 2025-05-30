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
    ADD CONSTRAINT payment_debtor_payment_id_fkey FOREIGN KEY (payment_id) REFERENCES public.payment(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.payment_debtor
    ADD CONSTRAINT payment_debtor_person_id_fkey FOREIGN KEY (person_id) REFERENCES public.person(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.payment_debtor
    ADD CONSTRAINT payment_debtor_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_manage ON public.payment_debtor TO administrator USING (true);
CREATE POLICY current_tenant ON public.payment_debtor AS RESTRICTIVE USING ((tenant_id = ( SELECT public.current_tenant_id() AS current_tenant_id)));
CREATE POLICY member_view ON public.payment_debtor FOR SELECT TO member USING (true);
