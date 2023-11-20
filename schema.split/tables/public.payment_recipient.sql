CREATE TABLE public.payment_recipient (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    payment_id bigint NOT NULL,
    account_id bigint NOT NULL,
    amount numeric(19,4) NOT NULL
);

COMMENT ON TABLE public.payment_recipient IS '@omit create,update,delete
@simpleCollections only';

GRANT ALL ON TABLE public.payment_recipient TO anonymous;
ALTER TABLE public.payment_recipient ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.payment_recipient
    ADD CONSTRAINT payment_recipient_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.payment_recipient
    ADD CONSTRAINT payment_recipient_account_id_fkey FOREIGN KEY (account_id) REFERENCES public.account(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.payment_recipient
    ADD CONSTRAINT payment_recipient_payment_id_fkey FOREIGN KEY (payment_id) REFERENCES public.payment(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.payment_recipient
    ADD CONSTRAINT payment_recipient_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_manage ON public.payment_recipient TO administrator USING (true);
CREATE POLICY my_tenant ON public.payment_recipient AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id()));
CREATE POLICY person_view ON public.payment_recipient FOR SELECT TO anonymous USING (true);

