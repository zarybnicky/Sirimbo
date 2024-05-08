CREATE TABLE public.payment (
    id bigint NOT NULL,
    tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL,
    accounting_period_id bigint NOT NULL,
    cohort_subscription_id bigint,
    event_registration_id bigint,
    event_instance_id bigint,
    status public.payment_status NOT NULL,
    variable_symbol text,
    specific_symbol text,
    is_auto_credit_allowed boolean DEFAULT true NOT NULL,
    tags text[] DEFAULT ARRAY[]::text[] NOT NULL,
    due_at timestamp with time zone,
    paid_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);

COMMENT ON TABLE public.payment IS '@omit create,delete
@simpleCollections both';

GRANT ALL ON TABLE public.payment TO anonymous;
ALTER TABLE public.payment ENABLE ROW LEVEL SECURITY;

ALTER TABLE ONLY public.payment
    ADD CONSTRAINT payment_pkey PRIMARY KEY (id);
ALTER TABLE ONLY public.payment
    ADD CONSTRAINT payment_accounting_period_id_fkey FOREIGN KEY (accounting_period_id) REFERENCES public.accounting_period(id);
ALTER TABLE ONLY public.payment
    ADD CONSTRAINT payment_cohort_subscription_id_fkey FOREIGN KEY (cohort_subscription_id) REFERENCES public.cohort_subscription(id) ON UPDATE CASCADE ON DELETE SET NULL;
ALTER TABLE ONLY public.payment
    ADD CONSTRAINT payment_event_instance_id_fkey FOREIGN KEY (event_instance_id) REFERENCES public.event_instance(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.payment
    ADD CONSTRAINT payment_event_registration_id_fkey FOREIGN KEY (event_registration_id) REFERENCES public.event_registration(id) ON UPDATE CASCADE ON DELETE CASCADE;
ALTER TABLE ONLY public.payment
    ADD CONSTRAINT payment_tenant_id_fkey FOREIGN KEY (tenant_id) REFERENCES public.tenant(id) ON UPDATE CASCADE ON DELETE CASCADE;

CREATE POLICY admin_manage ON public.payment TO administrator USING (true);
CREATE POLICY my_tenant ON public.payment AS RESTRICTIVE USING ((tenant_id = public.current_tenant_id()));
CREATE POLICY person_view ON public.payment FOR SELECT TO anonymous USING (true);

CREATE TRIGGER _100_timestamps BEFORE INSERT OR UPDATE ON public.payment FOR EACH ROW EXECUTE FUNCTION app_private.tg__timestamps();
CREATE TRIGGER _200_fill_accounting_period BEFORE INSERT ON public.payment FOR EACH ROW EXECUTE FUNCTION app_private.tg_payment__fill_accounting_period();
